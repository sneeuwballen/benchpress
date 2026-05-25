(* This file is free software. See file "license" for more details. *)

(** {1 Lua API surface for benchpress config files}

    Registers OCaml-backed functions under the [benchpress] global table so that
    config scripts can call [benchpress.prover{...}], [benchpress.dir{...}],
    etc.

    The state is threaded through a mutable accumulator [Lua_engine.state] so
    that all registrations in a file end up in one [Definitions.t]. *)

open Common
module Lua = Lua_api_lib
module LuaL = Lua_aux_lib
module Log = (val Logs.src_log (Logs.Src.create "benchpress.lua-api"))

(* ------------------------------------------------------------------ *)
(* Decodable record types ([@deriving ezlua]) *)

type custom_tag = { name: string; regex: string } [@@deriving ezlua]

type prover_opts = {
  name: string;
  binary: string option;
  cmd: string option;
  sat: string option;
  unsat: string option;
  unknown: string option;
  timeout: string option;
  memory: string option;
  produces_proof: bool option;
  proof_ext: string option;
  version: string option;
  binary_deps: string list option;
  static_labels: string list option;
  tags: custom_tag list option;
  ulimit_time: bool option;
  ulimit_memory: bool option;
  ulimit_stack: bool option;
}
[@@deriving ezlua]

type set_options = { j: int option; progress: bool option } [@@deriving ezlua]

(* ------------------------------------------------------------------ *)
(* Helpers *)

let get_cur_dir (st : Lua.state) : string =
  Lua.getglobal st "benchpress";
  if not (Lua.istable st (-1)) then (
    Lua.pop st 1;
    ""
  ) else (
    Lua.getfield st (-1) "cur_dir";
    let v =
      match Lua.tostring st (-1) with
      | Some s -> s
      | None -> ""
    in
    Lua.pop st 2;
    v
  )

let expand_cur_dir (st : Lua.state) (s : string) : string =
  let cur_dir = get_cur_dir st in
  Misc.str_replace [ "cur_dir", cur_dir ] s

(* ------------------------------------------------------------------ *)
(* Registration state *)

type pending = {
  mutable provers: Prover.t With_loc.t list;
  mutable dirs: Dir.t list;
  mutable tasks: Task.t With_loc.t list;
  mutable hooks: Lua_hooks.t;
  mutable checkers: Proof_checker.t With_loc.t list;
  mutable option_j: int option;
  mutable option_progress: bool option;
}

let make_pending hooks =
  {
    provers = [];
    dirs = [];
    tasks = [];
    hooks;
    checkers = [];
    option_j = None;
    option_progress = None;
  }

(* ------------------------------------------------------------------ *)
(* Lua function implementations *)

let bp_prover (pending : pending) (st : Lua_api_lib.state) : int =
  (try
     if not (Lua.istable st 1) then
       Error.fail "benchpress.prover: expected a table argument";
     let opts =
       match of_lua_prover_opts st 1 with
       | Ok o -> o
       | Error (`Msg e) -> Error.failf "benchpress.prover: %s" e
     in
     let binary =
       match opts.binary with
       | Some s -> expand_cur_dir st s
       | None -> opts.name
     in
     let cmd =
       match opts.cmd with
       | Some s -> expand_cur_dir st s
       | None -> ""
     in
     let static_labels = CCOpt.get_or ~default:[] opts.static_labels in
     (* cmd_fn: check if "cmd" field is a function *)
     let cmd_fn =
       Lua.getfield st 1 "cmd";
       let is_fn = Lua.isfunction st (-1) in
       Lua.pop st 1;
       if is_fn then (
         Lua.getfield st 1 "cmd";
         let r = Lua_hooks.save_fn_ref st in
         Some
           (fun (ctx : Prover.cmd_ctx) ->
             Lua_hooks.push_fn_ref st r;
             Lua.newtable st;
             Ezlua.push_field st "binary" Ezlua.Encode.string ctx.binary;
             Ezlua.push_field st "file" Ezlua.Encode.string ctx.file;
             Ezlua.push_field st "timeout" Ezlua.Encode.int ctx.timeout;
             Ezlua.push_field st "memory" Ezlua.Encode.int ctx.memory;
             CCOpt.iter
               (fun pf ->
                 Ezlua.push_field st "proof_file" Ezlua.Encode.string pf)
               ctx.proof_file;
             let status = Lua.pcall st 1 1 0 in
             match status with
             | Lua_api_lib.LUA_OK ->
               if Lua.isstring st (-1) then (
                 let s = Lua.tostring st (-1) |> CCOpt.get_or ~default:"" in
                 Lua.pop st 1;
                 Prover.Shell s
               ) else if Lua.istable st (-1) then (
                 let arr =
                   Ezlua.Decode.array Ezlua.Decode.string st (-1)
                   |> Ezlua.unwrap_err
                 in
                 Lua.pop st 1;
                 Prover.Exec arr
               ) else (
                 Lua.pop st 1;
                 Error.failf
                   "benchpress.prover %s: cmd function must return a string or \
                    array"
                   opts.name
               )
             | _ ->
               let msg =
                 Lua.tostring st (-1) |> CCOpt.get_or ~default:"(no message)"
               in
               Lua.pop st 1;
               Error.failf "benchpress.prover %s: cmd function error: %s"
                 opts.name msg)
       ) else
         None
     in
     (* analyze_fn: check if "parse" field is a function *)
     let analyze_fn =
       Lua.getfield st 1 "parse";
       let is_fn = Lua.isfunction st (-1) in
       Lua.pop st 1;
       if is_fn then (
         Lua.getfield st 1 "parse";
         let r = Lua_hooks.save_fn_ref st in
         Some
           (fun ~stdout ~stderr ->
             Lua_hooks.push_fn_ref st r;
             Ezlua.Encode.string st stdout;
             Ezlua.Encode.string st stderr;
             let status = Lua.pcall st 2 1 0 in
             match status with
             | Lua_api_lib.LUA_OK ->
               if Lua.isnoneornil st (-1) then (
                 Lua.pop st 1;
                 None
               ) else if not (Lua.istable st (-1)) then (
                 Lua.pop st 1;
                 Log.warn (fun k ->
                     k "prover %s: parse function must return a table or nil"
                       opts.name);
                 None
               ) else (
                 let res_str =
                   Ezlua.get_field st (-1) "res" Ezlua.Decode.string |> function
                   | Ok s -> s
                   | Error _ -> "unknown"
                 in
                 let labels =
                   Ezlua.get_field st (-1) "labels"
                     (Ezlua.Decode.option
                        (Ezlua.Decode.string_table Ezlua.Decode.bool))
                   |> function
                   | Ok (Some tbl) ->
                     List.filter_map
                       (fun (k, v) ->
                         if v then
                           Some k
                         else
                           None)
                       tbl
                   | _ -> []
                 in
                 Lua.pop st 1;
                 let res =
                   match res_str with
                   | "sat" -> Res.Sat
                   | "unsat" -> Res.Unsat
                   | "unknown" -> Res.Unknown
                   | "timeout" -> Res.Timeout
                   | "error" -> Res.Error
                   | tag -> Res.Tag tag
                 in
                 Some (res, labels)
               )
             | _ ->
               let msg =
                 Lua.tostring st (-1) |> CCOpt.get_or ~default:"(no message)"
               in
               Lua.pop st 1;
               Log.warn (fun k ->
                   k "prover %s: parse function error: %s" opts.name msg);
               None)
       ) else
         None
     in
     let custom =
       match opts.tags with
       | Some tags -> List.map (fun t -> t.name, t.regex) tags
       | None -> []
     in
     (* get_checkers: "proof_checker" can be a string or a function *)
     let decode_check_entry st : (string * string option) option =
       if Lua.isstring st (-1) then (
         let s = Lua.tostring st (-1) |> CCOpt.get_or ~default:"" in
         Lua.pop st 1;
         if s = "" then
           None
         else
           Some (s, None)
       ) else if Lua.istable st (-1) then (
         let open Ezlua.Decode in
         let r =
           let open Ezlua.Decode in
           let* checker = Ezlua.get_field st (-1) "checker" string in
           let* file = Ezlua.get_field st (-1) "file" (option string) in
           Ok (checker, file)
         in
         Lua.pop st 1;
         match r with
         | Ok (cname, fopt) ->
           if cname = "" then
             None
           else
             Some (cname, fopt)
         | Error _ -> None
       ) else (
         Lua.pop st 1;
         None
       )
     in
     let call_get_checkers_fn st r ~stdout ~stderr ~res ~proof_file =
       Lua_hooks.push_fn_ref st r;
       Lua.newtable st;
       Ezlua.push_field st "result" Ezlua.Encode.string (Res.to_string res);
       Ezlua.push_field st "stdout" Ezlua.Encode.string stdout;
       Ezlua.push_field st "stderr" Ezlua.Encode.string stderr;
       CCOpt.iter
         (fun pf -> Ezlua.push_field st "proof_file" Ezlua.Encode.string pf)
         proof_file;
       let status = Lua.pcall st 1 1 0 in
       if status = Lua_api_lib.LUA_OK then
         if Lua.isnoneornil st (-1) || not (Lua.istable st (-1)) then (
           Lua.pop st 1;
           []
         ) else (
           let n = Lua.objlen st (-1) in
           let acc = ref [] in
           for i = n downto 1 do
             Lua.rawgeti st (-1) i;
             CCOpt.iter (fun e -> acc := e :: !acc) (decode_check_entry st)
           done;
           Lua.pop st 1;
           !acc
         )
       else (
         let msg = Lua.tostring st (-1) |> CCOpt.get_or ~default:"(no msg)" in
         Lua.pop st 1;
         Log.warn (fun k ->
             k "prover %s: proof_checker function error: %s" opts.name msg);
         []
       )
     in
     Lua.getfield st 1 "proof_checker";
     let get_checkers =
       if Lua.isstring st (-1) then (
         let checker_name = Lua.tostring st (-1) |> CCOpt.get_or ~default:"" in
         Lua.pop st 1;
         if checker_name = "" then
           None
         else
           Some
             (fun ~stdout:_ ~stderr:_ ~res:_ ~proof_file:_ ->
               [ checker_name, None ])
       ) else if Lua.isfunction st (-1) then (
         let r = Lua_hooks.save_fn_ref st in
         Some
           (fun ~stdout ~stderr ~res ~proof_file ->
             call_get_checkers_fn st r ~stdout ~stderr ~res ~proof_file)
       ) else (
         Lua.pop st 1;
         None
       )
     in
     let version =
       match opts.version with
       | Some s -> Prover.Tag s
       | None -> Prover.Tag "<unknown>"
     in
     let ulimits =
       Ulimit.mk
         ~time:(CCOpt.get_or ~default:true opts.ulimit_time)
         ~memory:(CCOpt.get_or ~default:true opts.ulimit_memory)
         ~stack:(CCOpt.get_or ~default:false opts.ulimit_stack)
     in
     let prover : Prover.t =
       {
         name = opts.name;
         binary;
         cmd;
         cmd_fn;
         binary_deps = CCOpt.get_or ~default:[] opts.binary_deps;
         version;
         produces_proof = CCOpt.get_or ~default:false opts.produces_proof;
         proof_ext = opts.proof_ext;
         get_checkers;
         ulimits;
         sat = opts.sat;
         unsat = opts.unsat;
         unknown = opts.unknown;
         timeout = opts.timeout;
         memory = opts.memory;
         custom;
         static_labels;
         analyze_fn;
         defined_in = None;
         inherits = None;
       }
     in
     pending.provers <- With_loc.make ~loc:Loc.none prover :: pending.provers
   with Error.E e ->
     let msg = Error.show e in
     Ezlua.signal_error st msg);
  0

(* benchpress.dir { path, pattern, expect } *)
let bp_dir (pending : pending) (st : Lua_api_lib.state) : int =
  (try
     if not (Lua.istable st 1) then
       Error.fail "benchpress.dir: expected a table argument";
     let path =
       (match Ezlua.get_field st 1 "path" Ezlua.Decode.string with
       | Ok s -> s
       | Error (`Msg e) -> Error.failf "benchpress.dir: field 'path': %s" e)
       |> expand_cur_dir st |> Misc.mk_abs_path
     in
     let pattern =
       Ezlua.get_field st 1 "pattern" (Ezlua.Decode.option Ezlua.Decode.string)
       |> function
       | Ok x -> x
       | Error _ -> None
     in
     let expect_str =
       Ezlua.get_field st 1 "expect" (Ezlua.Decode.option Ezlua.Decode.string)
       |> function
       | Ok (Some s) -> s
       | _ -> "comment"
     in
     let expect =
       match expect_str with
       | "comment" -> Dir.E_comment
       | "unknown" -> Dir.E_const Res.Unknown
       | "sat" -> Dir.E_const Res.Sat
       | "unsat" -> Dir.E_const Res.Unsat
       | "error" -> Dir.E_const Res.Error
       | "timeout" -> Dir.E_const Res.Timeout
       | other ->
         Log.warn (fun k ->
             k "benchpress.dir: unknown expect value %S, defaulting to comment"
               other);
         Dir.E_comment
     in
     let name =
       Ezlua.get_field st 1 "name" (Ezlua.Decode.option Ezlua.Decode.string)
       |> function
       | Ok x -> x
       | Error _ -> None
     in
     let dir : Dir.t = { name; path; expect; pattern; loc = Loc.none } in
     pending.dirs <- dir :: pending.dirs
   with Error.E e ->
     let msg = Error.show e in
     Ezlua.signal_error st msg);
  0

(* ------------------------------------------------------------------ *)
(* Action builders *)

type action_box = Action.t ref

let push_action (st : Lua_api_lib.state) (a : Action.t) : unit =
  let box = ref a in
  Lua.pushlightuserdata st box

let pop_action_opt (st : Lua_api_lib.state) idx : Action.t option =
  match Lua.touserdata st idx with
  | Some (`Light_userdata p) ->
    (try Some !(Obj.obj (Obj.repr p) : action_box) with _ -> None)
  | _ -> None

(* benchpress.run_provers { provers, dirs, timeout, memory, j, pattern } *)
let bp_run_provers (pending : pending) (st : Lua_api_lib.state) : int =
  (try
     if not (Lua.istable st 1) then
       Error.fail "benchpress.run_provers: expected a table argument";
     let prover_names =
       match
         Ezlua.get_field st 1 "provers" (Ezlua.Decode.list Ezlua.Decode.string)
       with
       | Ok l -> l
       | Error (`Msg e) -> Error.failf "benchpress.run_provers: %s" e
     in
     let dir_paths =
       Ezlua.get_field st 1 "dirs"
         (Ezlua.Decode.option (Ezlua.Decode.list Ezlua.Decode.string))
       |> function
       | Ok (Some l) -> l
       | _ -> []
     in
     let timeout_s =
       Ezlua.get_field st 1 "timeout" (Ezlua.Decode.option Ezlua.Decode.int)
       |> function
       | Ok x -> x
       | Error _ -> None
     in
     let memory_mb =
       Ezlua.get_field st 1 "memory" (Ezlua.Decode.option Ezlua.Decode.int)
       |> function
       | Ok x -> x
       | Error _ -> None
     in
     let j =
       Ezlua.get_field st 1 "j" (Ezlua.Decode.option Ezlua.Decode.int)
       |> function
       | Ok x -> x
       | Error _ -> None
     in
     let pattern =
       Ezlua.get_field st 1 "pattern" (Ezlua.Decode.option Ezlua.Decode.string)
       |> function
       | Ok x -> x
       | Error _ -> None
     in
     let find_prover name =
       match
         List.find_opt
           (fun (p : Prover.t With_loc.t) -> p.view.name = name)
           pending.provers
       with
       | Some p -> p.view
       | None ->
         Error.failf "benchpress.run_provers: prover %S not defined" name
     in
     let provers = List.map find_prover prover_names in
     let find_registered_dir path =
       let path = Misc.mk_abs_path path in
       match List.find_opt (fun (d : Dir.t) -> d.path = path) pending.dirs with
       | Some d -> d
       | None ->
         (match
            List.find_opt
              (fun (d : Dir.t) ->
                CCString.prefix ~pre:(d.path ^ "/") path
                || CCString.prefix ~pre:d.path path)
              pending.dirs
          with
         | Some d -> d
         | None ->
           {
             Dir.name = None;
             path;
             expect = Dir.E_comment;
             pattern = None;
             loc = Loc.none;
           })
     in
     let dirs =
       List.map
         (fun path ->
           let path = expand_cur_dir st path |> Misc.mk_abs_path in
           let inside = find_registered_dir path in
           { Subdir.path; inside; loc = Loc.none })
         dir_paths
     in
     let limits =
       Limit.All.mk
         ?time:(CCOpt.map (fun s -> Limit.Time.mk ~s ()) timeout_s)
         ?memory:(CCOpt.map (fun m -> Limit.Memory.mk ~m ()) memory_mb)
         ()
     in
     let action : Action.t =
       Action.Act_run_provers
         { j; dirs; provers; pattern; limits; loc = Some Loc.none }
     in
     push_action st action
   with Error.E e ->
     let msg = Error.show e in
     Ezlua.signal_error st msg);
  1

(* benchpress.seq(a1, a2, ...) *)
let bp_seq (_pending : pending) (st : Lua_api_lib.state) : int =
  (try
     let nargs = Lua.gettop st in
     let actions =
       List.filter_map
         (fun i ->
           match pop_action_opt st i with
           | Some a -> Some a
           | None ->
             Log.warn (fun k ->
                 k "benchpress.seq: argument %d is not an action" i);
             None)
         CCList.(1 -- nargs)
     in
     push_action st (Action.Act_progn actions)
   with Error.E e ->
     let msg = Error.show e in
     Ezlua.signal_error st msg);
  1

(* benchpress.run_cmd(s) *)
let bp_run_cmd (_pending : pending) (st : Lua_api_lib.state) : int =
  (try
     let cmd =
       Ezlua.Decode.string st 1 |> function
       | Ok s -> s
       | Error (`Msg e) -> Error.failf "benchpress.run_cmd: %s" e
     in
     push_action st (Action.Act_run_cmd { cmd; loc = Loc.none })
   with Error.E e ->
     let msg = Error.show e in
     Ezlua.signal_error st msg);
  1

(* benchpress.task { name, action, synopsis } *)
let bp_task (pending : pending) (st : Lua_api_lib.state) : int =
  (try
     if not (Lua.istable st 1) then
       Error.fail "benchpress.task: expected a table argument";
     let name =
       match Ezlua.get_field st 1 "name" Ezlua.Decode.string with
       | Ok s -> s
       | Error (`Msg e) -> Error.failf "benchpress.task: %s" e
     in
     let synopsis =
       Ezlua.get_field st 1 "synopsis" (Ezlua.Decode.option Ezlua.Decode.string)
       |> function
       | Ok x -> x
       | Error _ -> None
     in
     Lua.getfield st 1 "action";
     let action =
       match pop_action_opt st (-1) with
       | Some a -> a
       | None ->
         Error.failf
           "benchpress.task %S: 'action' must be the result of \
            benchpress.run_provers, benchpress.seq, etc."
           name
     in
     Lua.pop st 1;
     let task : Task.t = { Task.name; synopsis; action; defined_in = None } in
     pending.tasks <- With_loc.make ~loc:Loc.none task :: pending.tasks
   with Error.E e ->
     let msg = Error.show e in
     Ezlua.signal_error st msg);
  0

(* benchpress.on(event_name, fn) *)
let bp_on (pending : pending) (st : Lua_api_lib.state) : int =
  (try
     let event =
       Ezlua.Decode.string st 1 |> function
       | Ok s -> s
       | Error (`Msg e) ->
         Error.failf "benchpress.on: first argument must be a string: %s" e
     in
     if not (Lua.isfunction st 2) then
       Error.failf "benchpress.on: second argument must be a function";
     Lua.pushvalue st 2;
     let r = Lua_hooks.save_fn_ref st in
     Lua_hooks.register pending.hooks event r
   with Error.E e ->
     let msg = Error.show e in
     Ezlua.signal_error st msg);
  0

(* benchpress.proof_checker { name, cmd, valid, invalid } *)
let bp_proof_checker (pending : pending) (st : Lua_api_lib.state) : int =
  (try
     if not (Lua.istable st 1) then
       Error.fail "benchpress.proof_checker: expected a table argument";
     let name =
       match Ezlua.get_field st 1 "name" Ezlua.Decode.string with
       | Ok s -> s
       | Error (`Msg e) -> Error.failf "benchpress.proof_checker: %s" e
     in
     let cmd =
       (match Ezlua.get_field st 1 "cmd" Ezlua.Decode.string with
       | Ok s -> s
       | Error (`Msg e) -> Error.failf "benchpress.proof_checker: %s" e)
       |> expand_cur_dir st
     in
     let valid =
       match Ezlua.get_field st 1 "valid" Ezlua.Decode.string with
       | Ok s -> s
       | Error (`Msg e) -> Error.failf "benchpress.proof_checker: %s" e
     in
     let invalid =
       match Ezlua.get_field st 1 "invalid" Ezlua.Decode.string with
       | Ok s -> s
       | Error (`Msg e) -> Error.failf "benchpress.proof_checker: %s" e
     in
     let checker : Proof_checker.t = { name; cmd; valid; invalid } in
     pending.checkers <- With_loc.make ~loc:Loc.none checker :: pending.checkers
   with Error.E e ->
     let msg = Error.show e in
     Ezlua.signal_error st msg);
  0

(* benchpress.set_options { j, progress } *)
let bp_set_options (pending : pending) (st : Lua_api_lib.state) : int =
  (try
     if not (Lua.istable st 1) then
       Error.fail "benchpress.set_options: expected a table argument";
     let opts =
       match of_lua_set_options st 1 with
       | Ok o -> o
       | Error (`Msg e) -> Error.failf "benchpress.set_options: %s" e
     in
     (match opts.j with
     | Some j -> pending.option_j <- Some j
     | None -> ());
     match opts.progress with
     | Some b -> pending.option_progress <- Some b
     | None -> ()
   with Error.E e ->
     let msg = Error.show e in
     Ezlua.signal_error st msg);
  0

(* ------------------------------------------------------------------ *)
(* Table construction *)

let register_benchpress_global (st : Lua_api_lib.state) (pending : pending) :
    unit =
  let mk name impl =
    let fn st = impl pending st in
    Ezlua.add_function st ("_bp_" ^ name) fn
  in
  mk "prover" bp_prover;
  mk "proof_checker" bp_proof_checker;
  mk "dir" bp_dir;
  mk "run_provers" bp_run_provers;
  mk "seq" bp_seq;
  mk "run_cmd" bp_run_cmd;
  mk "task" bp_task;
  mk "on" bp_on;
  mk "set_options" bp_set_options;
  Ezlua.run st
    {|
benchpress = {
  prover        = _bp_prover,
  proof_checker = _bp_proof_checker,
  dir           = _bp_dir,
  run_provers   = _bp_run_provers,
  seq           = _bp_seq,
  run_cmd       = _bp_run_cmd,
  task          = _bp_task,
  on            = _bp_on,
  set_options   = _bp_set_options,
}
|}
  |> function
  | Ok () -> ()
  | Error (`Msg e) -> Error.failf "could not set up benchpress global: %s" e

(* ------------------------------------------------------------------ *)
(* Config file template with LuaLS / EmmyLua type annotations *)

let config_template =
  {|-- benchpress configuration file
-- Type annotations for LuaLS / lua-language-server (EmmyLua format).
-- Delete this block if you do not use a Lua LSP.

---@class BP.CmdCtx
---@field binary string        # resolved binary path
---@field file string          # problem file path
---@field timeout integer      # timeout in seconds (0 = none)
---@field memory integer       # memory limit in MB (0 = none)
---@field proof_file? string   # path where proof output should be written (nil if not applicable)

---@class BP.CustomTag
---@field name string          # tag name
---@field regex string         # Perl regex to match in prover output

---@class BP.ProverParams
---@field name string                    # prover identifier
---@field binary? string                 # binary path/name (default: same as name, looked up in $PATH)
---@field cmd string|fun(ctx:BP.CmdCtx):string|string[]  # command template or function returning shell command / argv
---@field sat? string                    # Perl regex matching SAT output
---@field unsat? string                  # Perl regex matching UNSAT output
---@field unknown? string                # Perl regex matching unknown output
---@field timeout? string                # Perl regex matching timeout output
---@field memory? string                 # Perl regex matching OOM output
---@field static_labels? string[]        # labels attached to every result
---@field parse? fun(stdout:string, stderr:string):{res:string,labels?:table<string,boolean>}|nil  # custom result parser
---@field produces_proof? boolean        # whether the prover emits proof files
---@field proof_ext? string              # file extension for proof files (e.g. "proof")
---@field proof_checker? string|fun(r:{result:string,stdout:string,stderr:string,proof_file:string?}):(string|{checker:string,file:string})[]  # checker(s) to run on proof files
---@field binary_deps? string[]          # extra required binaries
---@field version? string                # prover version string
---@field tags? BP.CustomTag[]           # custom result tags with regexes
---@field ulimit_time? boolean           # set ulimit for time (default: true)
---@field ulimit_memory? boolean         # set ulimit for memory (default: true)
---@field ulimit_stack? boolean          # set ulimit for stack (default: false)

---@class BP.ProofCheckerParams
---@field name string      # checker identifier
---@field cmd string       # command template using $proof_file and $file
---@field valid string     # Perl regex matching a valid-proof line in output
---@field invalid string   # Perl regex matching an invalid-proof line in output

---@class BP.DirParams
---@field path string                    # directory path (supports $cur_dir)
---@field pattern? string                # Perl regex to match filenames
---@field expect? "comment"|"unknown"|"sat"|"unsat"|"error"|"timeout"  # default expected result
---@field name? string                   # logical name for this directory

---@class BP.RunProversParams
---@field provers string[]               # prover names to run
---@field dirs string[]                  # directory paths or names
---@field timeout? integer               # per-problem timeout in seconds
---@field memory? integer                # per-problem memory limit in MB
---@field j? integer                     # parallel jobs
---@field pattern? string                # override file-matching pattern

---@class BP.TaskParams
---@field name string                    # task identifier
---@field action any                     # result of run_provers / seq / run_cmd
---@field synopsis? string               # short description shown in listings

---@class BP.SetOptionsParams
---@field j? integer                     # default parallelism
---@field progress? boolean              # show progress bar

---@class BP
---@field prover fun(p:BP.ProverParams)                    # register a prover definition
---@field proof_checker fun(p:BP.ProofCheckerParams)       # register a proof checker
---@field dir fun(p:BP.DirParams)                          # register a problem directory
---@field run_provers fun(p:BP.RunProversParams):any        # build a run-provers action
---@field seq fun(...):any                                  # sequence several actions
---@field run_cmd fun(cmd:string):any                       # build a shell-command action
---@field task fun(p:BP.TaskParams)                 # register a named task
---@field on fun(event:string, fn:function)         # register an event hook
---@field set_options fun(p:BP.SetOptionsParams)    # set global options
---@field cur_dir string                            # directory of this config file (read-only)

---@type BP
benchpress = benchpress

-- ── Provers ───────────────────────────────────────────────────────────────────

benchpress.prover {
  name    = "my-prover",
  binary  = "my-prover",    -- looked up in $PATH
  cmd     = "$binary $file",
  sat     = "^SAT",
  unsat   = "^UNSAT",
  unknown = "^UNKNOWN",
}

-- ── Problem directories ───────────────────────────────────────────────────────

benchpress.dir {
  path    = "$cur_dir/problems/",
  pattern = ".*\\.smt2",
}

-- ── Tasks ─────────────────────────────────────────────────────────────────────

benchpress.task {
  name   = "all",
  action = benchpress.run_provers {
    provers = { "my-prover" },
    dirs    = { "$cur_dir/problems/" },
    timeout = 30,
  },
}
|}

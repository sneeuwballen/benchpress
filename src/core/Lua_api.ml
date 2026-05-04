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
(* Helpers for reading Lua table fields *)

let get_str_field st idx name = Ezlua.get_field st idx name Ezlua.Decode.string

(* Decode an optional field: returns [Ok None] when the field is nil/absent,
   [Ok (Some x)] when present and decodable, [Error _] on type mismatch. *)
let field_opt dec st idx name =
  Lua.getfield st idx name;
  let result =
    if Lua.isnoneornil st (-1) then
      Ok None
    else (
      match dec st (-1) with
      | Ok x -> Ok (Some x)
      | Error e -> Error e
    )
  in
  Lua.pop st 1;
  result

let get_str_field_opt st idx name = field_opt Ezlua.Decode.string st idx name

let get_str_list_field st idx name =
  Ezlua.get_field st idx name (Ezlua.Decode.list Ezlua.Decode.string)

let get_str_list_field_opt st idx name =
  field_opt (Ezlua.Decode.list Ezlua.Decode.string) st idx name

let get_int_field_opt st idx name = field_opt Ezlua.Decode.int st idx name
let get_bool_field_opt st idx name = field_opt Ezlua.Decode.bool st idx name

let unwrap_field name = function
  | Ok x -> x
  | Error (`Msg e) -> Error.failf "benchpress config: field %s: %s" name e

(* Read benchpress.cur_dir from the Lua state; returns "" if not set. *)
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

(* Expand $cur_dir in a string using the current Lua state. *)
let expand_cur_dir (st : Lua.state) (s : string) : string =
  let cur_dir = get_cur_dir st in
  Misc.str_replace [ "cur_dir", cur_dir ] s

(* ------------------------------------------------------------------ *)
(* Registration state (set before loading a file) *)

type pending = {
  mutable provers: Prover.t With_loc.t list;
  mutable dirs: Dir.t list;
  mutable tasks: Task.t With_loc.t list;
  mutable hooks: Lua_hooks.t;
}

let make_pending hooks = { provers = []; dirs = []; tasks = []; hooks }

(* ------------------------------------------------------------------ *)
(* Lua function implementations *)

(* benchpress.prover { name, binary, cmd, static_labels, parse, ... } *)
let bp_prover (pending : pending) (st : Lua_api_lib.state) : int =
  (try
     if not (Lua.istable st 1) then
       Error.fail "benchpress.prover: expected a table argument";
     let name = unwrap_field "name" (get_str_field st 1 "name") in
     let binary =
       get_str_field st 1 "binary" |> function
       | Ok s -> expand_cur_dir st s
       | Error _ -> name
     in
     let cmd =
       get_str_field st 1 "cmd" |> function
       | Ok s -> expand_cur_dir st s
       | Error _ -> ""
     in
     let static_labels =
       get_str_list_field_opt st 1 "static_labels" |> function
       | Ok (Some l) -> l
       | _ -> []
     in
     (* cmd_fn: check if "cmd" field is a function *)
     let cmd_fn =
       Lua.getfield st 1 "cmd";
       let is_fn = Lua.isfunction st (-1) in
       Lua.pop st 1;
       if is_fn then (
         (* Save the function reference *)
         Lua.getfield st 1 "cmd";
         let r = Lua_hooks.save_fn_ref st in
         Some
           (fun (ctx : Prover.cmd_ctx) ->
             Lua_hooks.push_fn_ref st r;
             (* Push context table *)
             Lua.newtable st;
             Ezlua.push_field st "binary" Ezlua.Encode.string ctx.binary;
             Ezlua.push_field st "file" Ezlua.Encode.string ctx.file;
             Ezlua.push_field st "timeout" Ezlua.Encode.int ctx.timeout;
             Ezlua.push_field st "memory" Ezlua.Encode.int ctx.memory;
             let status = Lua.pcall st 1 1 0 in
             match status with
             | Lua_api_lib.LUA_OK ->
               (* Result is either a string or a table of strings *)
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
                   name
               )
             | _ ->
               let msg =
                 Lua.tostring st (-1) |> CCOpt.get_or ~default:"(no message)"
               in
               Lua.pop st 1;
               Error.failf "benchpress.prover %s: cmd function error: %s" name
                 msg)
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
                       name);
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
                   k "prover %s: parse function error: %s" name msg);
               None)
       ) else
         None
     in
     (* regex fields for backwards compatibility / sexp interop *)
     let sat =
       get_str_field_opt st 1 "sat" |> function
       | Ok x -> x
       | _ -> None
     in
     let unsat =
       get_str_field_opt st 1 "unsat" |> function
       | Ok x -> x
       | _ -> None
     in
     let unknown =
       get_str_field_opt st 1 "unknown" |> function
       | Ok x -> x
       | _ -> None
     in
     let timeout_re =
       get_str_field_opt st 1 "timeout" |> function
       | Ok x -> x
       | _ -> None
     in
     let memory =
       get_str_field_opt st 1 "memory" |> function
       | Ok x -> x
       | _ -> None
     in
     let produces_proof =
       get_bool_field_opt st 1 "produces_proof" |> function
       | Ok (Some b) -> b
       | _ -> false
     in
     let proof_ext =
       get_str_field_opt st 1 "proof_ext" |> function
       | Ok x -> x
       | _ -> None
     in
     let proof_checker =
       get_str_field_opt st 1 "proof_checker" |> function
       | Ok x -> x
       | _ -> None
     in
     let binary_deps =
       get_str_list_field_opt st 1 "binary_deps" |> function
       | Ok (Some l) -> l
       | _ -> []
     in
     let prover : Prover.t =
       {
         name;
         binary;
         cmd;
         cmd_fn;
         binary_deps;
         version = Prover.Tag "<lua>";
         produces_proof;
         proof_ext;
         proof_checker;
         ulimits = Ulimit.mk ~time:true ~memory:true ~stack:false;
         sat;
         unsat;
         unknown;
         timeout = timeout_re;
         memory;
         custom = [];
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
       unwrap_field "path" (get_str_field st 1 "path")
       |> expand_cur_dir st |> Misc.mk_abs_path
     in
     let pattern =
       get_str_field_opt st 1 "pattern" |> function
       | Ok x -> x
       | _ -> None
     in
     let expect_str =
       get_str_field st 1 "expect" |> function
       | Ok s -> s
       | Error _ -> "comment"
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
       get_str_field_opt st 1 "name" |> function
       | Ok x -> x
       | _ -> None
     in
     let dir : Dir.t = { name; path; expect; pattern; loc = Loc.none } in
     pending.dirs <- dir :: pending.dirs
   with Error.E e ->
     let msg = Error.show e in
     Ezlua.signal_error st msg);
  0

(* ------------------------------------------------------------------ *)
(* Action builders: push an Action.t as a light userdata *)

(* We box Action.t values in a ref so they survive GC as light userdata.
   Light userdata in Lua 5.1 is just a pointer — using a heap-allocated
   ref keeps the value alive as long as we hold it in OCaml. *)

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
       unwrap_field "provers" (get_str_list_field st 1 "provers")
     in
     let dir_paths =
       get_str_list_field_opt st 1 "dirs" |> function
       | Ok (Some l) -> l
       | _ -> []
     in
     let timeout_s =
       get_int_field_opt st 1 "timeout" |> function
       | Ok x -> x
       | _ -> None
     in
     let memory_mb =
       get_int_field_opt st 1 "memory" |> function
       | Ok x -> x
       | _ -> None
     in
     let j =
       get_int_field_opt st 1 "j" |> function
       | Ok x -> x
       | _ -> None
     in
     let pattern =
       get_str_field_opt st 1 "pattern" |> function
       | Ok x -> x
       | _ -> None
     in
     (* Resolve provers from pending list *)
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
     (* Build subdirs: try to look up registered Dir.t by name or path prefix
        so that pattern/expect settings from benchpress.dir are preserved. *)
     let find_registered_dir path =
       let path = Misc.mk_abs_path path in
       (* exact path match first *)
       match List.find_opt (fun (d : Dir.t) -> d.path = path) pending.dirs with
       | Some d -> d
       | None ->
         (* fall back: a registered dir whose path is a prefix of [path] *)
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
     let name = unwrap_field "name" (get_str_field st 1 "name") in
     let synopsis =
       get_str_field_opt st 1 "synopsis" |> function
       | Ok x -> x
       | _ -> None
     in
     (* Get the action — it's stored as a light userdata in the "action" field *)
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
     (* Push the function and save it *)
     Lua.pushvalue st 2;
     let r = Lua_hooks.save_fn_ref st in
     Lua_hooks.register pending.hooks event r
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
  mk "dir" bp_dir;
  mk "run_provers" bp_run_provers;
  mk "seq" bp_seq;
  mk "run_cmd" bp_run_cmd;
  mk "task" bp_task;
  mk "on" bp_on;
  (* Assemble the benchpress table from the auto-wrapped _bp_* functions.
     add_function already installed _ez_check wrappers, so no manual glue needed. *)
  Ezlua.run st
    {|
benchpress = {
  prover      = _bp_prover,
  dir         = _bp_dir,
  run_provers = _bp_run_provers,
  seq         = _bp_seq,
  run_cmd     = _bp_run_cmd,
  task        = _bp_task,
  on          = _bp_on,
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

---@class BP.ProverParams
---@field name string                    # prover identifier
---@field binary? string                 # binary path/name (default: same as name, looked up in $PATH)
---@field cmd string                     # command template, e.g. "$binary $file"
---@field sat? string                    # Perl regex matching SAT output
---@field unsat? string                  # Perl regex matching UNSAT output
---@field unknown? string                # Perl regex matching unknown output
---@field timeout? string                # Perl regex matching timeout output
---@field memory? string                 # Perl regex matching OOM output
---@field static_labels? string[]        # labels attached to every result
---@field parse? fun(stdout:string, stderr:string):{res:string,labels?:table<string,boolean>}|nil  # custom result parser
---@field produces_proof? boolean        # whether the prover emits proof files
---@field proof_ext? string              # file extension for proof files (e.g. "proof")
---@field proof_checker? string          # name of prover used to check proofs
---@field binary_deps? string[]          # extra required binaries

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

---@class BP
---@field prover fun(p:BP.ProverParams)             # register a prover definition
---@field dir fun(p:BP.DirParams)                   # register a problem directory
---@field run_provers fun(p:BP.RunProversParams):any # build a run-provers action
---@field seq fun(...):any                           # sequence several actions
---@field run_cmd fun(cmd:string):any               # build a shell-command action
---@field task fun(p:BP.TaskParams)                 # register a named task
---@field on fun(event:string, fn:function)         # register an event hook
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

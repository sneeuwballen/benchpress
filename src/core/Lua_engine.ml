(* This file is free software. See file "license" for more details. *)

(** {1 Lua engine for benchpress config files} *)

open Common
module Log = (val Logs.src_log (Logs.Src.create "benchpress.lua-engine"))

type t = { state: Lua_api_lib.state; pending: Lua_api.pending }

let create () : t =
  let state = Ezlua.create ~stdlib:true () in
  let hooks = Lua_hooks.create state in
  let pending = Lua_api.make_pending hooks in
  Lua_api.register_benchpress_global state pending;
  (match Ezlua.run state Builtin_config.config with
  | Ok () -> ()
  | Error (`Msg e) -> Error.failf "loading builtin config: %s" e);
  { state; pending }

let load_file (t : t) (path : string) : unit =
  Log.debug (fun k -> k "loading lua config: %s" path);
  (* expose cur_dir as a global so scripts can reference their own directory *)
  let cur_dir =
    let abs = Misc.mk_abs_path path in
    Filename.dirname abs
  in
  (match
     Ezlua.run t.state (Printf.sprintf "benchpress.cur_dir = %S" cur_dir)
   with
  | Ok () -> ()
  | Error (`Msg e) ->
    Error.failf "error setting cur_dir for lua config %s: %s" path e);
  match Ezlua.run_file t.state path with
  | Ok () -> ()
  | Error (`Msg e) -> Error.failf "error loading lua config %s: %s" path e

let to_definitions (t : t) : Definitions.t =
  let open Definitions in
  let with_provers =
    List.fold_left
      (fun defs p -> add_prover p defs)
      empty
      (List.rev t.pending.Lua_api.provers)
  in
  let with_dirs =
    List.fold_left
      (fun defs d -> add_dir d defs)
      with_provers
      (List.rev t.pending.Lua_api.dirs)
  in
  let with_checkers =
    List.fold_left
      (fun defs c -> add_proof_checker c defs)
      with_dirs
      (List.rev t.pending.Lua_api.checkers)
  in
  List.fold_left
    (fun defs task -> add_task task defs)
    with_checkers
    (List.rev t.pending.Lua_api.tasks)

let hooks (t : t) : Lua_hooks.t = t.pending.Lua_api.hooks

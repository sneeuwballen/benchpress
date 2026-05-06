(* This file is free software. See file "license" for more details. *)

(** {1 Lua hook registry} *)

module Lua = Lua_api_lib
module LuaL = Lua_aux_lib

type lua_state = Lua_api_lib.state

type fn_ref = int
(** A saved reference into the Lua registry (via [luaL_ref]). *)

type t = { state: lua_state; hooks: (string, fn_ref list) Hashtbl.t }

let create state : t = { state; hooks = Hashtbl.create 8 }

(** Save the function at the top of the stack into the Lua registry and remove
    it from the stack. *)
let save_fn_ref (st : lua_state) : fn_ref = LuaL.ref_ st Lua.registryindex

(** Push a previously saved function reference back onto the stack. *)
let push_fn_ref (st : lua_state) (r : fn_ref) : unit =
  Lua.rawgeti st Lua.registryindex r

(** Register [fn_ref] for [event]. *)
let register (t : t) (event : string) (fn_ref : fn_ref) : unit =
  let prev = try Hashtbl.find t.hooks event with Not_found -> [] in
  Hashtbl.replace t.hooks event (prev @ [ fn_ref ])

(** Dispatch [event]. [encode] should push a single Lua table (the event
    payload) onto the stack. Each registered handler is called with that table
    as its only argument. *)
let dispatch (t : t) (event : string) (encode : lua_state -> unit) : unit =
  match Hashtbl.find_opt t.hooks event with
  | None | Some [] -> ()
  | Some refs ->
    List.iter
      (fun r ->
        push_fn_ref t.state r;
        encode t.state;
        let status = Lua.pcall t.state 1 0 0 in
        match status with
        | Lua_api_lib.LUA_OK -> ()
        | _ ->
          let msg =
            match Lua.tostring t.state (-1) with
            | Some s -> s
            | None -> "(no message)"
          in
          Lua.pop t.state 1;
          Logs.warn (fun k -> k "lua hook '%s' raised an error: %s" event msg))
      refs

(* This file is free software. See file "license" for more details. *)

(** Database error handling utilities *)

module Db = Sqlite3_utils

(** Check whether the DB contains a table named [tbl] *)
let has_table db (tbl : string) : bool =
  try
    CCResult.is_ok @@ Db.check_ret () @@ Sqlite3.finalize
    @@ Sqlite3.prepare db (Printf.sprintf "select count(*) from %s" tbl)
  with _ -> false

let err_of (e : Db.Rc.t) : Error.t =
  Error.makef "DB error: %s" @@ Db.Rc.to_string e

let unwrap msg (x : ('a, Db.Rc.t) result) : 'a =
  match x with
  | Ok x -> x
  | Error e ->
    let err = err_of e |> Error.wrap (msg ()) in
    Error.raise err

let unwrap_str msg (x : ('a, string) result) : 'a =
  match x with
  | Ok x -> x
  | Error e ->
    let err = Error.make e |> Error.wrap (msg ()) in
    Error.raise err

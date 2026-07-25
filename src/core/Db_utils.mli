(** Database utilities and error handling *)

val has_table : Sqlite3.db -> string -> bool
(** Check whether the DB contains a table *)

val err_of : ?db:Sqlite3.db -> Sqlite3_utils.Rc.t -> Error.t
(** Convert a database error to an Error.t. If [db] is provided, the SQLite
    error message is included. *)

val unwrap :
  ?db:Sqlite3.db -> (unit -> string) -> ('a, Sqlite3_utils.Rc.t) result -> 'a
(** Unwrap a database result, raising an error on failure. If [db] is provided,
    the SQLite error message is included in the context. *)

val unwrap_str : (unit -> string) -> ('a, string) result -> 'a
(** Unwrap a string result, raising an error on failure *)

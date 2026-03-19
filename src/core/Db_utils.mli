(** Database utilities and error handling *)

(** Check whether the DB contains a table *)
val has_table : Sqlite3.db -> string -> bool

(** Convert a database error to an Error.t *)
val err_of : Sqlite3_utils.Rc.t -> Error.t

(** Unwrap a database result, raising an error on failure *)
val unwrap : (unit -> string) -> ('a, Sqlite3_utils.Rc.t) result -> 'a

(** Unwrap a string result, raising an error on failure *)
val unwrap_str : (unit -> string) -> ('a, string) result -> 'a

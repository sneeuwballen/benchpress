(** Thin wrapper for [Sqlite3] with a simplified interface

@see < https://bitbucket.org/mmottl/sqlite3-ocaml > sqlite3-ocaml
@see < http://mmottl.bitbucket.org/projects/sqlite3-ocaml/api/Sqlite3.html > Sqlite3 (sqlite3-ocaml documentation)
*)

module Sqlite3 = Sqlite3

module Data = Sqlite3.Data
module Rc = Sqlite3.Rc

(** raised whenever an SQLite operation returns an unsuccessful return code *)
exception Rc of Rc.t

(** raised when we are trying to handle an exception and another exception is
    raised (e.g. we are executing a transaction, encounter an exception, and issue
    a ROLLBACK command, which itself fails) *)
exception Finally of exn*exn

type t
(** A connection to a DB *)

val open_ :
  ?mode:[ `READONLY | `NO_CREATE] -> ?mutex:[ `FULL | `NO ] ->
  ?cache:[ `PRIVATE | `SHARED ] -> ?vfs:string ->
  string -> t
(** [as Sqlite3.db_open] *)

(** immediately close the database. The database connection will otherwise be
    closed when garbage-collected. *)
val close : t -> unit

(** [with_db filename f] opens a database, applies [f], and returns the
    result. The database is closed after [f] is evaluated, even if it raises an
    exception. *)
val with_open :
  ?mode:[ `READONLY | `NO_CREATE] -> ?mutex:[ `FULL | `NO ] ->
  ?cache:[ `PRIVATE | `SHARED ] -> ?vfs:string ->
  string -> (t -> 'a) -> 'a

(** [transact db f] evaluates [f db] within a BEGIN..COMMIT transaction.

    If [f db] evaluates successfully to [y], the transaction is committed and
    [y] is returned. If the evaluation of [f db] raises an exception, the
    transaction is rolled back and the exception is re-raised.

    Note that BEGIN..COMMIT transactions cannot be nested in SQLite. Any attempt
    to make a nested call to [transact] will raise an exception. *)
val transact : t -> (t -> 'a) -> 'a

(** [atomically db f] evaluates [f db] within a SAVEPOINT..RELEASE
    transaction, which may be nested.

    This implementation allows only parenthetically nested transactions, so there
    is no need to name savepoints. *)
val atomically : t -> (t -> 'a) -> 'a

(** execute some imperative SQL statement(s). Multiple statements may be
    separated by a semicolon. *)
val exec : t -> string -> unit

(* TODO: one shot query *)

(** as [Sqlite3.last_insert_rowid] *)
val last_insert_rowid : t -> Int64.t

(** as [Sqlite3.changes] *)
val changes : t -> int

(** Values representing types to pass to a statement, or to extract from 
    a row *)
module Ty : sig
  type ('a, 'res) t

  val return : ('res, 'res) t
  val int : ('a, 'res) t -> (int -> 'a, 'res) t
  val int64 : ('a, 'res) t -> (int64 -> 'a, 'res) t
  val float : ('a, 'res) t -> (float -> 'a, 'res) t
  val string : ('a, 'res) t -> (string -> 'a, 'res) t
  val data : ('a, 'res) t -> (Data.t -> 'a, 'res) t
end

(** a compiled statement, can be either imperative or a query.
    @param 'a is the first argument of the {!Ty.t} used for the parameters
    of the query.
    @param 'b is the first argument of the {!Ty.t} used to extract data from
    the rows returned by the query.
*)
type ('a, 'b, 'res) statement

(** prepare a statement from the SQL text. The given string must contain only
    one SQL statement. The statement can be used multiple times (calls to
    [Sqlite3.reset] are handled automatically). The statement is not actually
    compiled until its first use. *)
val make_statement : t -> string ->
  ('a, 'res) Ty.t ->
  ('b, 'res) Ty.t ->
  ('a,'b,'res) statement

(** bind the given parameters and execute an imperative statement. An
    exception is raised if the statement attempts to return result rows. *)
val statement_exec : ('a,unit,unit) statement -> 'a

(** [statement_query stmt ~f ~init params] binds the given parameters
    and executes a query. Each result row is passed to [f], along with the
    accumulator that starts at [init]. *)
val statement_query_fold :
  f:'cb -> ('q, 'cb, init:'res -> 'res) statement -> 'q

(** [statement_query stmt params cons fold init] binds the given parameters
    and executes a query. Each result row is first passed to [cons], which will
    usually construct a value from the data. This value is then passed to [fold]
    along with an intermediate value, which is [init] for the first row. This can
    used to build a list or other data structure from all the results. *)
val statement_query_iter : f:'cb -> ('q,'cb,unit) statement -> 'q

(** immediately finalize the statement, releasing any associated resources.
    The statement will otherwise be finalized when garbage-collected. *)
val statement_finalize : _ statement -> unit

(**/**)
val db_handle : t -> Sqlite3.db
(**/**)

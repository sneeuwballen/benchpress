
(** Analyze results of one file *)

open Misc
open Test

type t = {
  improved  : int;
  ok        : int;
  disappoint: int;
  bad       : int; (* mismatch *)
  bad_full  : (Problem.t * Res.t * float) list; (* always materialized *)
  errors    : int;
  errors_full : (Problem.t * Res.t * float) list;
  total     : int;
}

val of_db_for : ?full:bool -> Db.t -> prover:Prover.name -> t or_error
val of_db : ?full:bool -> Db.t -> (Prover.name * t) list or_error

val of_db_dirs : Db.t -> string list or_error
val of_db_n_bad : Db.t -> int or_error
(** Compute number of bad results *)

val to_printbox_l : ?link:prover_string_linker ->
  (Prover.name * t) list -> PrintBox.t
val to_printbox_bad : ?link:path_linker -> t -> PrintBox.t
val to_printbox_bad_l :
  ?link:prover_path_linker ->
  (Prover.name * t) list -> (string*string list*PrintBox.t) list
val to_printbox_errors : ?link:path_linker -> t -> PrintBox.t
val to_printbox_errors_l :
  ?link:prover_path_linker ->
  (Prover.name * t) list -> (string*string list*PrintBox.t) list

val is_ok : t -> bool

val num_bad : t -> int

val pp : t Fmt.printer
val pp_bad : t Fmt.printer

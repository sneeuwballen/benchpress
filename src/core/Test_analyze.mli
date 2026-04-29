(** Analyze results of one file *)

open Common
open Test

type t = {
  improved: int;
  ok: int;
  disappoint: int;
  bad: int; (* mismatch *)
  bad_full: (Problem.t * Res.t * float) list; (* always materialized *)
  valid_proof: int;
  invalid_proof: int;
  invalid_proof_full: (Problem.t * Proof_check_res.t * float) list;
  errors: int;
  errors_full: (Problem.t * Res.t * float) list;
  total: int;
}

val of_db_dirs : Db.t -> string list
val of_db_n_bad : Db.t -> int
val of_events_for : prover:Prover.name -> Run_event.t list -> t
val of_events : ?full:bool -> Run_event.t list -> (Prover.name * t) list
val of_events_n_bad : Run_event.t list -> int
val of_events_dirs : Run_event.t list -> string list

val to_printbox_l :
  ?link:prover_string_linker -> (Prover.name * t) list -> PrintBox.t

val to_printbox_bad : ?link:path_linker -> t -> PrintBox.t

val to_printbox_bad_l :
  ?link:prover_path_linker ->
  (Prover.name * t) list ->
  (string * string list * PrintBox.t) list

val to_printbox_errors : ?link:path_linker -> t -> PrintBox.t

val to_printbox_errors_l :
  ?link:prover_path_linker ->
  (Prover.name * t) list ->
  (string * string list * PrintBox.t) list

val to_printbox_invalid_proof_l :
  ?link:prover_path_linker ->
  (Prover.name * t) list ->
  (string * string list * PrintBox.t) list

val is_ok : t -> bool
val num_bad : t -> int
val pp : t Fmt.printer
val pp_bad : t Fmt.printer

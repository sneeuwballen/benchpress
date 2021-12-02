
(** Basic stats on results *)

open Common
open Test

module PB = PrintBox

type t = {
  unsat: int;
  sat: int;
  errors: int;
  unknown: int;
  timeout: int;
  memory: int;
  custom: (string * int) list;
  total: int;
  total_time: float; (* for sat+unsat *)
}

val to_printbox_l :
  ?to_link:prover_string_linker ->
  (string*t) list ->
  PB.t

val of_db_for : prover:Prover.name -> Db.t -> t
val of_db : Db.t -> (Prover.name * t) list

val pp : t Fmt.printer

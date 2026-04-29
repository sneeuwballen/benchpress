(** Basic stats on results *)

open Common
open Test
module PB = PrintBox

type detail_stats = { n: int; total: float; mean: float; sd: float }

type t = {
  unsat: detail_stats;
  sat: detail_stats;
  errors: int;
  unknown: detail_stats;
  timeout: int;
  memory: int;
  valid_proof: int;
  invalid_proof: int;
  custom: (string * detail_stats) list;
  total: int;
  total_time: float; (* for sat+unsat *)
}

val to_printbox_l :
  ?details:bool -> ?to_link:prover_string_linker -> (string * t) list -> PB.t

val of_events_for : prover:Prover.name -> Run_event.t list -> t
val of_events : Run_event.t list -> (Prover.name * t) list
val pp : t Fmt.printer

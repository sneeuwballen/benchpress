(** Lightweight Comparison between bench runs *)

open Common

type single = { better: int; worse: int; same: int }

type t = {
  provers: Prover.name list;
  tbl: (Prover.name * Prover.name * single) list;
}

val of_events : Run_event.t list -> t
val to_printbox_l : t -> PrintBox.t

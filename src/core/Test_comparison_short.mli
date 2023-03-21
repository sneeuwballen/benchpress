(** Lightweight Comparison between bench runs *)

open Common

type single = { better: int; worse: int; same: int }

type t = {
  provers: Prover.name list;
  tbl: (Prover.name * Prover.name * single) list;
}

val of_db : Db.t -> t
val to_printbox_l : t -> PrintBox.t

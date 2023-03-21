(* This file is free software. See file "license" for more details. *)

(** {1 Event Stored on Disk or Transmitted on Network} *)

open Common

type prover = Prover.t
type checker = Proof_checker.t

type t =
  | Prover_run of (Prover.name, Res.t) Run_result.t
  | Checker_run of
      (Prover.name * Proof_checker.name, Proof_check_res.t) Run_result.t

type event = t

val mk_prover : (Prover.name, Res.t) Run_result.t -> t

val mk_checker :
  (Prover.name * Proof_checker.name, Proof_check_res.t) Run_result.t -> t

val pp : t CCFormat.printer
val db_prepare : Db.t -> unit
val to_db_prover_result : Db.t -> (Prover.name, Res.t) Run_result.t -> unit
val to_db : Db.t -> t -> unit
val of_db_l : Db.t -> t list

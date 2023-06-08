(* This file is free software. See file "license" for more details. *)

open Common

type path = string
type job_res = (Prover.name, Res.t) Run_result.t

type check_res =
  (Prover.name * Proof_checker.name, Proof_check_res.t) Run_result.t

val run :
  limits:Limit.All.t ->
  proof_file:string option ->
  Prover.t ->
  Problem.t ->
  job_res
(** Run this prover on this problem. *)

val run_proof_check :
  limits:Limit.All.t ->
  Prover.t ->
  Proof_checker.t ->
  Problem.t ->
  proof_file:string ->
  check_res

val is_bad : job_res -> bool
(** Is it a mismatch with the expected result, i.e a bug? *)

val pp_result : w_prover:int -> w_pb:int -> job_res Fmt.printer

val pp_result_progress : w_prover:int -> w_pb:int -> job_res -> unit
(** Clear line, print this on stdout *)

val pp_check_result : w_prover:int -> w_pb:int -> check_res Fmt.printer

val pp_check_result_progress : w_prover:int -> w_pb:int -> check_res -> unit
(** Clear line, print this on stdout *)

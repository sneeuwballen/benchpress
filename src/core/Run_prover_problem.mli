
(* This file is free software. See file "license" for more details. *)

open Common

type path = string
type job_res= (Prover.name, Res.t) Run_result.t

val run :
  limits:Limit.All.t ->
  Prover.t ->
  Problem.t ->
  job_res

val pp_result :
  w_prover:int ->
  w_pb:int ->
  job_res Fmt.printer

val pp_result_progress :
  w_prover:int ->
  w_pb:int ->
  job_res ->
  unit
(** Clear line, print this on stdout *)

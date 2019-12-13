
(* This file is free software. See file "license" for more details. *)

module Fmt = CCFormat
type 'a or_error = ('a, string) CCResult.t

type path = string
type job_res = Prover.name Run_result.t

val run :
  timeout:int ->
  memory:int ->
  Prover.t ->
  Problem.t ->
  job_res or_error

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

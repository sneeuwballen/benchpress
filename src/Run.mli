
(* This file is free software. See file "license" for more details. *)

type env = (string * string) array

(** {2 Main} *)

val run_proc : timeout:int -> string -> Event.raw_result

val run_prover :
  ?env:env ->
  timeout:int ->
  memory:int ->
  prover:Prover.t ->
  pb:Problem.t ->
  unit ->
  Event.prover Event.result
(** Runs the prover in a sub-process, and returns a the result *)

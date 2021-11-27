module Fmt = CCFormat

type +'a t = private {
  program : 'a;
  problem : Problem.t;
  res : Res.t;
  timeout : Limit.Time.t;
  raw : Run_proc_result.t;
}

val program : 'a t -> 'a
val problem : _ t -> Problem.t
val raw : _ t -> Run_proc_result.t

val map : f:('a -> 'b) -> 'a t -> 'b t

val make_from_prover :
  Prover.t ->
  timeout:Limit.Time.t ->
  Problem.t ->
  Run_proc_result.t ->
  Prover.name t
(** Analyze result *)

val analyze_self : Prover.t t -> Prover.name t

val make:
  Prover.name ->
  timeout:Limit.Time.t ->
  res:Res.t ->
  Problem.t ->
  Run_proc_result.t ->
  Prover.name t

val pp : 'a Fmt.printer -> 'a t Fmt.printer


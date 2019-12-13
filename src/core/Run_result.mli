module Fmt = CCFormat
module J = Misc.Json

type +'a t = private {
  program : 'a;
  problem : Problem.t;
  res : Res.t;
  timeout : int;
  raw : Proc_run_result.t;
}

val program : 'a t -> 'a
val problem : _ t -> Problem.t
val raw : _ t -> Proc_run_result.t

val map : f:('a -> 'b) -> 'a t -> 'b t

val make_from_prover :
  Prover.t ->
  timeout:int ->
  Problem.t ->
  Proc_run_result.t ->
  Prover.name t
(** Analyze result *)

val analyze_self : Prover.t t -> Prover.name t

val make:
  Prover.name ->
  timeout:int ->
  res:Res.t ->
  Problem.t ->
  Proc_run_result.t ->
  Prover.name t

val pp : 'a Fmt.printer -> 'a t Fmt.printer

val encode : 'a J.Encode.t -> 'a t J.Encode.t
val decode : 'a J.Decode.t -> 'a t J.Decode.t


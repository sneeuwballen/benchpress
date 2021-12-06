module Fmt = CCFormat

type (+'a, 'res) t = private {
  program : 'a;
  problem : Problem.t;
  res : 'res;
  timeout : Limit.Time.t;
  raw : Run_proc_result.t;
}

val program : ('a,_) t -> 'a
val problem : _ t -> Problem.t
val raw : _ t -> Run_proc_result.t

val map : f:('a -> 'b) -> ('a,'res) t -> ('b,'res) t

val make_from_prover :
  Prover.t ->
  timeout:Limit.Time.t ->
  Problem.t ->
  Run_proc_result.t ->
  (Prover.name, Res.t) t
(** Analyze result *)

val analyze_self : (Prover.t, Res.t) t -> (Prover.name, Res.t) t

val make:
  'name ->
  timeout:Limit.Time.t ->
  res:'res ->
  Problem.t ->
  Run_proc_result.t ->
  ('name, 'res) t

val make_from_checker :
  Prover.t ->
  Proof_checker.t ->
  timeout:Limit.Time.t ->
  Problem.t ->
  Run_proc_result.t ->
  (Prover.name * Proof_checker.name, Proof_check_res.t) t

val pp : 'a Fmt.printer -> 'res Fmt.printer -> ('a, 'res) t Fmt.printer


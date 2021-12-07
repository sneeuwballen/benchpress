
(** Detailed test results *)

open Common
open Test

type t = (Prover.t, Res.t) Run_result.t
(** Detailed result *)

type key = {
  prover: Prover.name;
  file: string;
  res: Res.t;
  file_expect: Res.t;
  rtime: float;
}
(** Summary of a result *)

(** Filter on detailed results *)
type expect_filter =
  | TD_expect_improved
  | TD_expect_ok
  | TD_expect_disappoint
  | TD_expect_bad
  | TD_expect_error

val list_keys :
  ?offset:int -> ?page_size:int ->
  ?filter_prover:string ->
  ?filter_pb:string ->
  ?filter_res:string ->
  ?filter_expect:expect_filter ->
  Db.t -> (key list * int * bool)
(** List available results.
    @returns tuple [l, n, is_done], where [is_done] is true if there are
    no more results, and [n] is the total number of results (not just
    those in [l]). *)

type proof_check_res = {
  res: Proof_check_res.t;
  stdout: string;
  rtime: float;
}

val to_printbox : ?link:prover_path_linker ->
  t -> proof_check_res option ->
  PrintBox.t * PrintBox.t * string * string * string option
(** Display an individual result + prover descr + stdout + stderr + proof stdout *)

val get_res : Db.t -> Prover.name -> string -> t * proof_check_res option
(** Get an individual result *)

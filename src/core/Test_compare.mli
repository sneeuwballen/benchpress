
(** {1 Compare two result files} *)

module Fmt = CCFormat
type 'a or_error = ('a, string) CCResult.t
type filename = string

module Short : sig
  type t = {
    appeared: int;  (* new problems *)
    disappeared: int; (* problems that disappeared *)
    improved: int;
    regressed: int;
    mismatch: int;
    same: int; (* same result *)
  }

  val to_printbox : t -> PrintBox.t

  val make : filename -> filename -> (Prover.name * t) list or_error
end

(* TODO
module Full : sig
  type t = {
    appeared: (Problem.t * Res.t) list;  (* new problems *)
    disappeared: (Problem.t * Res.t) list; (* problems that disappeared *)
    improved: (Problem.t * Res.t * Res.t) list;
    regressed: (Problem.t * Res.t * Res.t) list;
    mismatch: (Problem.t * Res.t * Res.t) list;
    same: (Problem.t * Res.t * float * float) list; (* same result *)
  }

  val to_printbox : t -> PrintBox.t
  val to_printbox_l : (Prover.name * t) list -> PrintBox.t

  val make : filename -> filename -> (Prover.name * t) list or_error
end
   *)

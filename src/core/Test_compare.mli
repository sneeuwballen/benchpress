(** Compare two result files *)

type filename = string
type prover = filename * Prover.name
type status = [ `Sat | `Unsat ]

module Short : sig
  type t = {
    appeared: int; (* new problems *)
    disappeared: int; (* problems that disappeared *)
    improved: int;
    regressed: int;
    mismatch: int;
    same: int; (* same result *)
  }

  val to_printbox : t -> PrintBox.t
  val make : ?status:status -> filename -> filename -> (Prover.name * t) list

  val make_provers : ?status:status -> prover -> prover -> t
  (** Make a single comparison between two provers in (possibly) different files *)
end

module Full : sig
  type filter = [ `Improved | `Regressed | `Mismatch | `Same ]
  type entry = string * Res.t * float * Res.t * float

  val make_filtered :
    ?page:int ->
    ?page_size:int ->
    ?filter:filter ->
    ?status:status ->
    prover ->
    prover ->
    entry list

  val to_printbox :
    ?file_link:(string -> string -> PrintBox.t) -> entry list -> PrintBox.t
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

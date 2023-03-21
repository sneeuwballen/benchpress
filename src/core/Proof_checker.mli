open Common

type name = string

type t = {
  name: name;
  cmd: string; (* take $proof_file and $problem *)
  valid: string;
  invalid: string;
}

val pp : t Fmt.printer

exception Subst_not_found of string

val make_cmd :
  ?env:(string * string) array ->
  problem:string ->
  proof_file:string ->
  t ->
  string
(** Interpolate a command using the given substitution function.
    @raise Subst_not_found if a variable is found, that is not substituted
    into any of the parameters nor by [f] *)

module Res = Proof_check_res

val run :
  ?limits:Limit.All.t ->
  problem:string ->
  proof_file:string ->
  t ->
  Run_proc_result.t

val analyze_res : t -> Run_proc_result.t -> Res.t option
val db_prepare : Db.t -> unit
val to_db : Db.t -> t -> unit
val of_db : Db.t -> string -> t

val db_names : Db.t -> string list
(** Names of checkers in this DB *)

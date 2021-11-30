
open Common
type 'a or_error = 'a Or_error.t

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
  t -> string
(** Interpolate a command using the given substitution function.
    @raise Subst_not_found if a variable is found, that is not substituted
    into any of the parameters nor by [f] *)

module Res = Proof_check_res

val run :
  problem:string -> proof_file:string ->
  t ->
  Run_proc_result.t

val analyze_res :
  t -> Run_proc_result.t -> Res.t

val db_prepare : Db.t -> unit or_error

val to_db : Db.t -> t -> unit or_error

val of_db : Db.t -> string -> t or_error

val db_names : Db.t -> string list or_error
(** Names of checkers in this DB *)

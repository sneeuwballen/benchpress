(* This file is free software. See file "license" for more details. *)

(** {1 Definitions} *)

open Common

type path = string
type 'a with_loc = 'a With_loc.t
type t

type def =
  | D_prover of Prover.t with_loc
  | D_task of Task.t with_loc
  | D_proof_checker of Proof_checker.t with_loc

val empty : t
val find_prover : t -> string -> Prover.t with_loc
val find_prover' : t -> string -> Prover.t
val find_checker : t -> string -> Proof_checker.t with_loc
val find_task : t -> string -> Task.t with_loc
val find_task' : t -> string -> Task.t
val find : t -> string -> def option
val errors : t -> Error.t list
val to_iter : t -> (string * def) Iter.t
val all_provers : t -> Prover.t with_loc list
val all_checkers : t -> Proof_checker.t with_loc list
val all_tasks : t -> Task.t with_loc list
val custom_tags : t -> string list

module Def : sig
  type t = def

  val loc : t -> Loc.t
  val pp : t Fmt.printer
  val show : t -> string
end

val option_j : t -> int option
val option_progress : t -> bool option
val add_stanza : ?reify_errors:bool -> Stanza.t -> t -> t
val add_stanza_l : ?reify_errors:bool -> Stanza.t list -> t -> t
val of_stanza_l : ?reify_errors:bool -> Stanza.t list -> t
val mk_subdir : t -> string -> Subdir.t
val mk_paths : ?dir_files:string list -> string list -> string list

val mk_run_provers :
  ?j:int ->
  ?timeout:int ->
  ?memory:int ->
  ?stack:Stanza.stack_limit ->
  ?pattern:string ->
  paths:path list ->
  provers:string list ->
  loc:Loc.t option ->
  t ->
  Action.run_provers
(** Build a "run" action from the given prover names
    and directory paths.
    All the provers must be defined, and the paths must be contained
    in declared [dir]. *)

val mk_run_provers_slurm_submission :
  ?j:int ->
  paths:path list ->
  ?timeout:int ->
  ?memory:int ->
  ?stack:Stanza.stack_limit ->
  ?pattern:path ->
  provers:path list ->
  ?loc:Loc.t ->
  ?partition:path ->
  ?nodes:int ->
  ?addr:Unix.inet_addr ->
  ?port:int ->
  ?ntasks:int ->
  t ->
  Action.run_provers_slurm_submission

val completions : t -> ?before_pos:Loc.pos -> string -> def list
(** Find possible completions *)

(* TODO:
   val pp : t Fmt.printer *)

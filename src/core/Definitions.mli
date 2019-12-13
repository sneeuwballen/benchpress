
(* This file is free software. See file "license" for more details. *)

(** {1 Definitions} *)

module E = CCResult
module Fmt = CCFormat
type path = string
type 'a or_error = ('a, string) CCResult.t

type t

type def =
  | D_prover of Prover.t
  | D_task of Task.t

val empty : t

val find_prover : t -> string -> Prover.t or_error
val find_task : t -> string -> Task.t or_error
val all_provers : t -> Prover.t list

val of_config : Config.t -> t or_error
[@@ocaml.deprecated "use sexp stanzas"]
(** Get a list of supported provers from a config file. *)

val add_stanza : Stanza.t -> t -> t or_error

val add_stanza_l : Stanza.t list -> t -> t or_error

val of_stanza_l : Stanza.t list -> t or_error

val mk_subdir : t -> string -> Subdir.t or_error

val mk_run_provers :
  ?j:int ->
  ?timeout:int ->
  ?memory:int ->
  ?pattern:string ->
  paths:path list ->
  provers:string list ->
  t ->
  Action.run_provers or_error
(** Build a "run" action from the given prover names
    and directory paths.
    All the provers must be defined, and the paths must be contained
    in declared [dir]. *)

(* TODO:
   val pp : t Fmt.printer *)


(* This file is free software. See file "license" for more details. *)

module StrMap = Misc.Str_map

type 'a or_error = ('a, string) CCResult.t

val of_config : Config.t -> Prover.t StrMap.t or_error
(** Get a list of supported provers from a config file. *)

val find_config : Config.t -> string -> Prover.t or_error
(** Parse prover description from config file, and check it is listed
    in the "provers" list *)

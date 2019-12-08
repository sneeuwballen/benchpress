(* This file is free software. See file "license" for more details. *)

(** {1 Run Prover}

    Utils to run a theorem prover (or a similar tool) and extract its result
*)

module Fmt = CCFormat

(** {2 Prover configurations} *)

type version =
  | Tag of string
  | Git of {
      branch: string;
      commit: string;  (* branch & commit hash *)
    }

type t = {
  (* Prover identification *)
  name : string;
  version : version;

  (* Pover execution *)
  binary: string;       (* name of the program itself *)
  binary_deps: string list; (* list of binaries this depends on *)
  cmd: string;          (* the command line to run.
                           possibly contains $binary, $file, $memory and $timeout *)

  (* Result analysis *)
  unsat   : string option;  (* regex for "unsat" *)
  sat     : string option;  (* regex for "sat" *)
  unknown : string option;  (* regex for "unknown" *)
  timeout : string option;  (* regex for "timeout" *)
  memory  : string option;  (* regex for "out of memory" *)
}
(** The type of provers configurations *)

val name : t -> string
(** Prover name *)

val pp_name : t Fmt.printer
val pp : t Fmt.printer
val pp_version : version Fmt.printer
val version_to_string : version -> string

val equal : t -> t -> bool
(** Equality (by name) *)

exception Subst_not_found of string

val interpolate_cmd :
  ?env:(string * string) array ->
  ?binary:string ->
  ?timeout:int -> ?memory:int -> ?file:string ->
  ?f:(string -> string option) ->
  string -> string
(** Interpolate the given parameters (env, timeout, memory, etc.)
    in the given string.
    @param f called for other interpolations
    @raise Subst_not_found if a variable is found, that is not substituted
    into any of the parameters nor by [f] *)

val make_command :
  ?env:(string * string) array ->
  t ->
  timeout:int ->
  memory:int ->
  file:string ->
  string

val run :
  ?env:(string * string) array ->
  timeout:int ->
  memory:int ->
  file:string ->
  t ->
  Proc_run_result.t

val analyze_p_opt : t -> Proc_run_result.t -> Res.t option
(** Analyze raw result to look for the result *)

(** Map by name *)
module Map_name : CCMap.S with type key = t

(** Map with full compare *)
module Map : CCMap.S with type key = t
module Set : CCSet.S with type elt = t

module J = Misc.Json

val encode_version : version J.Encode.t
val decode_version : version J.Decode.t
val encode : t J.Encode.t
val decode : t J.Decode.t

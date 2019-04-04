
(* This file is free software. See file "license" for more details. *)

(** {1 Run Prover}

    Utils to run a theorem prover (or a similar tool) and extract its result
*)

(** {2 Prover configurations} *)

type version =
  | Tag of string
  | Git of string * string  (* branch & commit hash *)

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

val pp_name : Format.formatter -> t -> unit

val equal : t -> t -> bool
(** Equality (by name) *)

val make_command :
  ?env:(string * string) array ->
  t ->
  timeout:int ->
  memory:int ->
  file:string ->
  string

(** Map by name *)
module Map_name : CCMap.S with type key = t

(** Map with full compare *)
module Map : CCMap.S with type key = t
module Set : CCSet.S with type elt = t


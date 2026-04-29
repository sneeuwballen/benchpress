(* This file is free software. See file "license" for more details. *)

(** {1 Event Stored on Disk or Transmitted on Network} *)

open Common

type prover = Prover.t
type checker = Proof_checker.t

type t =
  | Prover_run of (Prover.name, Res.t) Run_result.t
  | Checker_run of
      (Prover.name * Proof_checker.name, Proof_check_res.t) Run_result.t

type event = t

val mk_prover : (Prover.name, Res.t) Run_result.t -> t

val mk_checker :
  (Prover.name * Proof_checker.name, Proof_check_res.t) Run_result.t -> t

val pp : t CCFormat.printer

(* --- Protobuf / JSON encoding --- *)

val to_event_pb : ?no_data:bool -> t -> Benchpress_core.event
(** Convert to a proto [Event]. If [~no_data:true], stdout/stderr are
    represented as sha256 refs (blobs not embedded). *)

val of_event_pb :
  read_zip_entry:(string -> bytes) -> Benchpress_core.event -> t option
(** Convert from a proto [Event]; returns [None] for non-result events
    (RunStart, etc.). [read_zip_entry name] is called to resolve sha256 data
    refs. *)

val to_json_line : ?no_data:bool -> t -> string
(** JSON-encode as a single line (no trailing newline). *)

val of_json_line : read_zip_entry:(string -> bytes) -> string -> t option
(** Parse a JSON line; returns [None] for non-result events. *)

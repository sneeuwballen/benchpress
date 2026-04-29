(** Code for benchpress_core.proto *)

(* generated from "benchpress_core.proto", do not edit *)

(** {2 Types} *)

type problem = private {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 2 fields *)
  mutable name: string;
  mutable expected: string;
}

type data_ref = Inline of bytes | Sha256ref of string

type run_proc_result = private {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 4 fields *)
  mutable errcode: int32;
  mutable stdout: data_ref option;
  mutable stderr: data_ref option;
  mutable rtime: float;
  mutable utime: float;
  mutable stime: float;
}

type run_start = private {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 2 fields *)
  mutable uuid: string;
  mutable timestamp: float;
  mutable dirs: string list;
  mutable provers: string list;
}

type run_done = private {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 3 fields *)
  mutable total_wall_time: float;
  mutable n_results: int32;
  mutable n_bad: int32;
}

type prover_start = private {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 2 fields *)
  mutable prover: string;
  mutable problem: problem option;
  mutable timeout_s: int64;
}

type prover_done = private {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 3 fields *)
  mutable prover: string;
  mutable problem: problem option;
  mutable res: string;
  mutable labels: string list;
  mutable timeout_s: int64;
  mutable raw: run_proc_result option;
}

type checker_start = private {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 2 fields *)
  mutable prover: string;
  mutable checker: string;
  mutable problem: problem option;
}

type checker_done = private {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 3 fields *)
  mutable prover: string;
  mutable checker: string;
  mutable problem: problem option;
  mutable proof_check_res: string;
  mutable raw: run_proc_result option;
}

type manifest = private {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 5 fields *)
  mutable uuid: string;
  mutable timestamp: string;
  mutable provers: string list;
  mutable total_wall_time: float;
  mutable n_results: int32;
  mutable n_bad: int32;
  mutable dirs: string list;
}

type event =
  | Run_start of run_start
  | Run_done of run_done
  | Prover_start of prover_start
  | Prover_done of prover_done
  | Checker_start of checker_start
  | Checker_done of checker_done

(** {2 Basic values} *)

val default_problem : unit -> problem
(** [default_problem ()] is a new empty value for type [problem] *)

val default_data_ref : unit -> data_ref
(** [default_data_ref ()] is a new empty value for type [data_ref] *)

val default_run_proc_result : unit -> run_proc_result
(** [default_run_proc_result ()] is a new empty value for type [run_proc_result]
*)

val default_run_start : unit -> run_start
(** [default_run_start ()] is a new empty value for type [run_start] *)

val default_run_done : unit -> run_done
(** [default_run_done ()] is a new empty value for type [run_done] *)

val default_prover_start : unit -> prover_start
(** [default_prover_start ()] is a new empty value for type [prover_start] *)

val default_prover_done : unit -> prover_done
(** [default_prover_done ()] is a new empty value for type [prover_done] *)

val default_checker_start : unit -> checker_start
(** [default_checker_start ()] is a new empty value for type [checker_start] *)

val default_checker_done : unit -> checker_done
(** [default_checker_done ()] is a new empty value for type [checker_done] *)

val default_manifest : unit -> manifest
(** [default_manifest ()] is a new empty value for type [manifest] *)

val default_event : unit -> event
(** [default_event ()] is a new empty value for type [event] *)

(** {2 Make functions} *)

val make_problem : ?name:string -> ?expected:string -> unit -> problem
(** [make_problem … ()] is a builder for type [problem] *)

val copy_problem : problem -> problem

val problem_has_name : problem -> bool
(** presence of field "name" in [problem] *)

val problem_set_name : problem -> string -> unit
(** set field name in problem *)

val problem_has_expected : problem -> bool
(** presence of field "expected" in [problem] *)

val problem_set_expected : problem -> string -> unit
(** set field expected in problem *)

val make_run_proc_result :
  ?errcode:int32 ->
  ?stdout:data_ref ->
  ?stderr:data_ref ->
  ?rtime:float ->
  ?utime:float ->
  ?stime:float ->
  unit ->
  run_proc_result
(** [make_run_proc_result … ()] is a builder for type [run_proc_result] *)

val copy_run_proc_result : run_proc_result -> run_proc_result

val run_proc_result_has_errcode : run_proc_result -> bool
(** presence of field "errcode" in [run_proc_result] *)

val run_proc_result_set_errcode : run_proc_result -> int32 -> unit
(** set field errcode in run_proc_result *)

val run_proc_result_has_stdout : run_proc_result -> bool
(** presence of field "stdout" in [run_proc_result] *)

val run_proc_result_set_stdout : run_proc_result -> data_ref -> unit
(** set field stdout in run_proc_result *)

val run_proc_result_has_stderr : run_proc_result -> bool
(** presence of field "stderr" in [run_proc_result] *)

val run_proc_result_set_stderr : run_proc_result -> data_ref -> unit
(** set field stderr in run_proc_result *)

val run_proc_result_has_rtime : run_proc_result -> bool
(** presence of field "rtime" in [run_proc_result] *)

val run_proc_result_set_rtime : run_proc_result -> float -> unit
(** set field rtime in run_proc_result *)

val run_proc_result_has_utime : run_proc_result -> bool
(** presence of field "utime" in [run_proc_result] *)

val run_proc_result_set_utime : run_proc_result -> float -> unit
(** set field utime in run_proc_result *)

val run_proc_result_has_stime : run_proc_result -> bool
(** presence of field "stime" in [run_proc_result] *)

val run_proc_result_set_stime : run_proc_result -> float -> unit
(** set field stime in run_proc_result *)

val make_run_start :
  ?uuid:string ->
  ?timestamp:float ->
  ?dirs:string list ->
  ?provers:string list ->
  unit ->
  run_start
(** [make_run_start … ()] is a builder for type [run_start] *)

val copy_run_start : run_start -> run_start

val run_start_has_uuid : run_start -> bool
(** presence of field "uuid" in [run_start] *)

val run_start_set_uuid : run_start -> string -> unit
(** set field uuid in run_start *)

val run_start_has_timestamp : run_start -> bool
(** presence of field "timestamp" in [run_start] *)

val run_start_set_timestamp : run_start -> float -> unit
(** set field timestamp in run_start *)

val run_start_set_dirs : run_start -> string list -> unit
(** set field dirs in run_start *)

val run_start_set_provers : run_start -> string list -> unit
(** set field provers in run_start *)

val make_run_done :
  ?total_wall_time:float -> ?n_results:int32 -> ?n_bad:int32 -> unit -> run_done
(** [make_run_done … ()] is a builder for type [run_done] *)

val copy_run_done : run_done -> run_done

val run_done_has_total_wall_time : run_done -> bool
(** presence of field "total_wall_time" in [run_done] *)

val run_done_set_total_wall_time : run_done -> float -> unit
(** set field total_wall_time in run_done *)

val run_done_has_n_results : run_done -> bool
(** presence of field "n_results" in [run_done] *)

val run_done_set_n_results : run_done -> int32 -> unit
(** set field n_results in run_done *)

val run_done_has_n_bad : run_done -> bool
(** presence of field "n_bad" in [run_done] *)

val run_done_set_n_bad : run_done -> int32 -> unit
(** set field n_bad in run_done *)

val make_prover_start :
  ?prover:string -> ?problem:problem -> ?timeout_s:int64 -> unit -> prover_start
(** [make_prover_start … ()] is a builder for type [prover_start] *)

val copy_prover_start : prover_start -> prover_start

val prover_start_has_prover : prover_start -> bool
(** presence of field "prover" in [prover_start] *)

val prover_start_set_prover : prover_start -> string -> unit
(** set field prover in prover_start *)

val prover_start_has_problem : prover_start -> bool
(** presence of field "problem" in [prover_start] *)

val prover_start_set_problem : prover_start -> problem -> unit
(** set field problem in prover_start *)

val prover_start_has_timeout_s : prover_start -> bool
(** presence of field "timeout_s" in [prover_start] *)

val prover_start_set_timeout_s : prover_start -> int64 -> unit
(** set field timeout_s in prover_start *)

val make_prover_done :
  ?prover:string ->
  ?problem:problem ->
  ?res:string ->
  ?labels:string list ->
  ?timeout_s:int64 ->
  ?raw:run_proc_result ->
  unit ->
  prover_done
(** [make_prover_done … ()] is a builder for type [prover_done] *)

val copy_prover_done : prover_done -> prover_done

val prover_done_has_prover : prover_done -> bool
(** presence of field "prover" in [prover_done] *)

val prover_done_set_prover : prover_done -> string -> unit
(** set field prover in prover_done *)

val prover_done_has_problem : prover_done -> bool
(** presence of field "problem" in [prover_done] *)

val prover_done_set_problem : prover_done -> problem -> unit
(** set field problem in prover_done *)

val prover_done_has_res : prover_done -> bool
(** presence of field "res" in [prover_done] *)

val prover_done_set_res : prover_done -> string -> unit
(** set field res in prover_done *)

val prover_done_set_labels : prover_done -> string list -> unit
(** set field labels in prover_done *)

val prover_done_has_timeout_s : prover_done -> bool
(** presence of field "timeout_s" in [prover_done] *)

val prover_done_set_timeout_s : prover_done -> int64 -> unit
(** set field timeout_s in prover_done *)

val prover_done_has_raw : prover_done -> bool
(** presence of field "raw" in [prover_done] *)

val prover_done_set_raw : prover_done -> run_proc_result -> unit
(** set field raw in prover_done *)

val make_checker_start :
  ?prover:string -> ?checker:string -> ?problem:problem -> unit -> checker_start
(** [make_checker_start … ()] is a builder for type [checker_start] *)

val copy_checker_start : checker_start -> checker_start

val checker_start_has_prover : checker_start -> bool
(** presence of field "prover" in [checker_start] *)

val checker_start_set_prover : checker_start -> string -> unit
(** set field prover in checker_start *)

val checker_start_has_checker : checker_start -> bool
(** presence of field "checker" in [checker_start] *)

val checker_start_set_checker : checker_start -> string -> unit
(** set field checker in checker_start *)

val checker_start_has_problem : checker_start -> bool
(** presence of field "problem" in [checker_start] *)

val checker_start_set_problem : checker_start -> problem -> unit
(** set field problem in checker_start *)

val make_checker_done :
  ?prover:string ->
  ?checker:string ->
  ?problem:problem ->
  ?proof_check_res:string ->
  ?raw:run_proc_result ->
  unit ->
  checker_done
(** [make_checker_done … ()] is a builder for type [checker_done] *)

val copy_checker_done : checker_done -> checker_done

val checker_done_has_prover : checker_done -> bool
(** presence of field "prover" in [checker_done] *)

val checker_done_set_prover : checker_done -> string -> unit
(** set field prover in checker_done *)

val checker_done_has_checker : checker_done -> bool
(** presence of field "checker" in [checker_done] *)

val checker_done_set_checker : checker_done -> string -> unit
(** set field checker in checker_done *)

val checker_done_has_problem : checker_done -> bool
(** presence of field "problem" in [checker_done] *)

val checker_done_set_problem : checker_done -> problem -> unit
(** set field problem in checker_done *)

val checker_done_has_proof_check_res : checker_done -> bool
(** presence of field "proof_check_res" in [checker_done] *)

val checker_done_set_proof_check_res : checker_done -> string -> unit
(** set field proof_check_res in checker_done *)

val checker_done_has_raw : checker_done -> bool
(** presence of field "raw" in [checker_done] *)

val checker_done_set_raw : checker_done -> run_proc_result -> unit
(** set field raw in checker_done *)

val make_manifest :
  ?uuid:string ->
  ?timestamp:string ->
  ?provers:string list ->
  ?total_wall_time:float ->
  ?n_results:int32 ->
  ?n_bad:int32 ->
  ?dirs:string list ->
  unit ->
  manifest
(** [make_manifest … ()] is a builder for type [manifest] *)

val copy_manifest : manifest -> manifest

val manifest_has_uuid : manifest -> bool
(** presence of field "uuid" in [manifest] *)

val manifest_set_uuid : manifest -> string -> unit
(** set field uuid in manifest *)

val manifest_has_timestamp : manifest -> bool
(** presence of field "timestamp" in [manifest] *)

val manifest_set_timestamp : manifest -> string -> unit
(** set field timestamp in manifest *)

val manifest_set_provers : manifest -> string list -> unit
(** set field provers in manifest *)

val manifest_has_total_wall_time : manifest -> bool
(** presence of field "total_wall_time" in [manifest] *)

val manifest_set_total_wall_time : manifest -> float -> unit
(** set field total_wall_time in manifest *)

val manifest_has_n_results : manifest -> bool
(** presence of field "n_results" in [manifest] *)

val manifest_set_n_results : manifest -> int32 -> unit
(** set field n_results in manifest *)

val manifest_has_n_bad : manifest -> bool
(** presence of field "n_bad" in [manifest] *)

val manifest_set_n_bad : manifest -> int32 -> unit
(** set field n_bad in manifest *)

val manifest_set_dirs : manifest -> string list -> unit
(** set field dirs in manifest *)

(** {2 Formatters} *)

val pp_problem : Format.formatter -> problem -> unit
(** [pp_problem v] formats v *)

val pp_data_ref : Format.formatter -> data_ref -> unit
(** [pp_data_ref v] formats v *)

val pp_run_proc_result : Format.formatter -> run_proc_result -> unit
(** [pp_run_proc_result v] formats v *)

val pp_run_start : Format.formatter -> run_start -> unit
(** [pp_run_start v] formats v *)

val pp_run_done : Format.formatter -> run_done -> unit
(** [pp_run_done v] formats v *)

val pp_prover_start : Format.formatter -> prover_start -> unit
(** [pp_prover_start v] formats v *)

val pp_prover_done : Format.formatter -> prover_done -> unit
(** [pp_prover_done v] formats v *)

val pp_checker_start : Format.formatter -> checker_start -> unit
(** [pp_checker_start v] formats v *)

val pp_checker_done : Format.formatter -> checker_done -> unit
(** [pp_checker_done v] formats v *)

val pp_manifest : Format.formatter -> manifest -> unit
(** [pp_manifest v] formats v *)

val pp_event : Format.formatter -> event -> unit
(** [pp_event v] formats v *)

(** {2 Protobuf Encoding} *)

val encode_pb_problem : problem -> Pbrt.Encoder.t -> unit
(** [encode_pb_problem v encoder] encodes [v] with the given [encoder] *)

val encode_pb_data_ref : data_ref -> Pbrt.Encoder.t -> unit
(** [encode_pb_data_ref v encoder] encodes [v] with the given [encoder] *)

val encode_pb_run_proc_result : run_proc_result -> Pbrt.Encoder.t -> unit
(** [encode_pb_run_proc_result v encoder] encodes [v] with the given [encoder]
*)

val encode_pb_run_start : run_start -> Pbrt.Encoder.t -> unit
(** [encode_pb_run_start v encoder] encodes [v] with the given [encoder] *)

val encode_pb_run_done : run_done -> Pbrt.Encoder.t -> unit
(** [encode_pb_run_done v encoder] encodes [v] with the given [encoder] *)

val encode_pb_prover_start : prover_start -> Pbrt.Encoder.t -> unit
(** [encode_pb_prover_start v encoder] encodes [v] with the given [encoder] *)

val encode_pb_prover_done : prover_done -> Pbrt.Encoder.t -> unit
(** [encode_pb_prover_done v encoder] encodes [v] with the given [encoder] *)

val encode_pb_checker_start : checker_start -> Pbrt.Encoder.t -> unit
(** [encode_pb_checker_start v encoder] encodes [v] with the given [encoder] *)

val encode_pb_checker_done : checker_done -> Pbrt.Encoder.t -> unit
(** [encode_pb_checker_done v encoder] encodes [v] with the given [encoder] *)

val encode_pb_manifest : manifest -> Pbrt.Encoder.t -> unit
(** [encode_pb_manifest v encoder] encodes [v] with the given [encoder] *)

val encode_pb_event : event -> Pbrt.Encoder.t -> unit
(** [encode_pb_event v encoder] encodes [v] with the given [encoder] *)

(** {2 Protobuf Decoding} *)

val decode_pb_problem : Pbrt.Decoder.t -> problem
(** [decode_pb_problem decoder] decodes a [problem] binary value from [decoder]
*)

val decode_pb_data_ref : Pbrt.Decoder.t -> data_ref
(** [decode_pb_data_ref decoder] decodes a [data_ref] binary value from
    [decoder] *)

val decode_pb_run_proc_result : Pbrt.Decoder.t -> run_proc_result
(** [decode_pb_run_proc_result decoder] decodes a [run_proc_result] binary value
    from [decoder] *)

val decode_pb_run_start : Pbrt.Decoder.t -> run_start
(** [decode_pb_run_start decoder] decodes a [run_start] binary value from
    [decoder] *)

val decode_pb_run_done : Pbrt.Decoder.t -> run_done
(** [decode_pb_run_done decoder] decodes a [run_done] binary value from
    [decoder] *)

val decode_pb_prover_start : Pbrt.Decoder.t -> prover_start
(** [decode_pb_prover_start decoder] decodes a [prover_start] binary value from
    [decoder] *)

val decode_pb_prover_done : Pbrt.Decoder.t -> prover_done
(** [decode_pb_prover_done decoder] decodes a [prover_done] binary value from
    [decoder] *)

val decode_pb_checker_start : Pbrt.Decoder.t -> checker_start
(** [decode_pb_checker_start decoder] decodes a [checker_start] binary value
    from [decoder] *)

val decode_pb_checker_done : Pbrt.Decoder.t -> checker_done
(** [decode_pb_checker_done decoder] decodes a [checker_done] binary value from
    [decoder] *)

val decode_pb_manifest : Pbrt.Decoder.t -> manifest
(** [decode_pb_manifest decoder] decodes a [manifest] binary value from
    [decoder] *)

val decode_pb_event : Pbrt.Decoder.t -> event
(** [decode_pb_event decoder] decodes a [event] binary value from [decoder] *)

(** {2 Protobuf YoJson Encoding} *)

val encode_json_problem : problem -> Yojson.Basic.t
(** [encode_json_problem v encoder] encodes [v] to to json *)

val encode_json_data_ref : data_ref -> Yojson.Basic.t
(** [encode_json_data_ref v encoder] encodes [v] to to json *)

val encode_json_run_proc_result : run_proc_result -> Yojson.Basic.t
(** [encode_json_run_proc_result v encoder] encodes [v] to to json *)

val encode_json_run_start : run_start -> Yojson.Basic.t
(** [encode_json_run_start v encoder] encodes [v] to to json *)

val encode_json_run_done : run_done -> Yojson.Basic.t
(** [encode_json_run_done v encoder] encodes [v] to to json *)

val encode_json_prover_start : prover_start -> Yojson.Basic.t
(** [encode_json_prover_start v encoder] encodes [v] to to json *)

val encode_json_prover_done : prover_done -> Yojson.Basic.t
(** [encode_json_prover_done v encoder] encodes [v] to to json *)

val encode_json_checker_start : checker_start -> Yojson.Basic.t
(** [encode_json_checker_start v encoder] encodes [v] to to json *)

val encode_json_checker_done : checker_done -> Yojson.Basic.t
(** [encode_json_checker_done v encoder] encodes [v] to to json *)

val encode_json_manifest : manifest -> Yojson.Basic.t
(** [encode_json_manifest v encoder] encodes [v] to to json *)

val encode_json_event : event -> Yojson.Basic.t
(** [encode_json_event v encoder] encodes [v] to to json *)

(** {2 JSON Decoding} *)

val decode_json_problem : Yojson.Basic.t -> problem
(** [decode_json_problem decoder] decodes a [problem] value from [decoder] *)

val decode_json_data_ref : Yojson.Basic.t -> data_ref
(** [decode_json_data_ref decoder] decodes a [data_ref] value from [decoder] *)

val decode_json_run_proc_result : Yojson.Basic.t -> run_proc_result
(** [decode_json_run_proc_result decoder] decodes a [run_proc_result] value from
    [decoder] *)

val decode_json_run_start : Yojson.Basic.t -> run_start
(** [decode_json_run_start decoder] decodes a [run_start] value from [decoder]
*)

val decode_json_run_done : Yojson.Basic.t -> run_done
(** [decode_json_run_done decoder] decodes a [run_done] value from [decoder] *)

val decode_json_prover_start : Yojson.Basic.t -> prover_start
(** [decode_json_prover_start decoder] decodes a [prover_start] value from
    [decoder] *)

val decode_json_prover_done : Yojson.Basic.t -> prover_done
(** [decode_json_prover_done decoder] decodes a [prover_done] value from
    [decoder] *)

val decode_json_checker_start : Yojson.Basic.t -> checker_start
(** [decode_json_checker_start decoder] decodes a [checker_start] value from
    [decoder] *)

val decode_json_checker_done : Yojson.Basic.t -> checker_done
(** [decode_json_checker_done decoder] decodes a [checker_done] value from
    [decoder] *)

val decode_json_manifest : Yojson.Basic.t -> manifest
(** [decode_json_manifest decoder] decodes a [manifest] value from [decoder] *)

val decode_json_event : Yojson.Basic.t -> event
(** [decode_json_event decoder] decodes a [event] value from [decoder] *)

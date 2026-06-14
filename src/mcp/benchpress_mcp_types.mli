
(** Code for benchpress_mcp_types.proto *)

(* generated from "benchpress_mcp_types.proto", do not edit *)



(** {2 Types} *)

type tools_capability = unit

type server_capabilities = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable tools : unit;
}

type server_info = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable name : string;
  mutable version : string;
}

type initialize_result = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable protocol_version : string;
  mutable capabilities : server_capabilities option;
  mutable server_info : server_info option;
}

type tool_def = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 3 fields *)
  mutable name : string;
  mutable description : string;
  mutable input_schema_json : string;
}

type tools_list_result = private {
  mutable tools : tool_def list;
}

type tool_call_result = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable content_json : string;
}

type job_summary = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 6 fields *)
  mutable file : string;
  mutable uuid : string;
  mutable timestamp : float;
  mutable n_results : int32;
  mutable n_bad : int32;
  mutable provers : string list;
  mutable size_bytes : int32;
}

type list_jobs_result = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable jobs : job_summary list;
  mutable offset : int32;
  mutable limit : int32;
}

type prover_stat = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 10 fields *)
  mutable prover : string;
  mutable sat : int32;
  mutable unsat : int32;
  mutable errors : int32;
  mutable timeout : int32;
  mutable unknown : int32;
  mutable memory : int32;
  mutable total : int32;
  mutable total_time : float;
  mutable total_time_solved : float;
}

type job_status_result = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 7 fields *)
  mutable file : string;
  mutable uuid : string;
  mutable timestamp : float;
  mutable total_wall_time : float;
  mutable n_results : int32;
  mutable n_bad : int32;
  mutable is_complete : bool;
  mutable provers : prover_stat list;
}

type result_entry = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 3 fields *)
  mutable prover : string;
  mutable result : string;
  mutable time_s : float;
}

type result_row = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable problem : string;
  mutable results : result_entry list;
}

type job_results_result = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable file : string;
  mutable provers : string list;
  mutable rows : result_row list;
  mutable n_rows : int32;
}

type query_item = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 5 fields *)
  mutable prover : string;
  mutable problem : string;
  mutable result : string;
  mutable expected : string;
  mutable time_s : float;
}

type query_job_results_result = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 5 fields *)
  mutable file : string;
  mutable total : int32;
  mutable page : int32;
  mutable per_page : int32;
  mutable is_complete : bool;
  mutable items : query_item list;
}


(** {2 Basic values} *)

val default_tools_capability : unit
(** [default_tools_capability] is the default value for type [tools_capability] *)

val default_server_capabilities : unit -> server_capabilities 
(** [default_server_capabilities ()] is a new empty value for type [server_capabilities] *)

val default_server_info : unit -> server_info 
(** [default_server_info ()] is a new empty value for type [server_info] *)

val default_initialize_result : unit -> initialize_result 
(** [default_initialize_result ()] is a new empty value for type [initialize_result] *)

val default_tool_def : unit -> tool_def 
(** [default_tool_def ()] is a new empty value for type [tool_def] *)

val default_tools_list_result : unit -> tools_list_result 
(** [default_tools_list_result ()] is a new empty value for type [tools_list_result] *)

val default_tool_call_result : unit -> tool_call_result 
(** [default_tool_call_result ()] is a new empty value for type [tool_call_result] *)

val default_job_summary : unit -> job_summary 
(** [default_job_summary ()] is a new empty value for type [job_summary] *)

val default_list_jobs_result : unit -> list_jobs_result 
(** [default_list_jobs_result ()] is a new empty value for type [list_jobs_result] *)

val default_prover_stat : unit -> prover_stat 
(** [default_prover_stat ()] is a new empty value for type [prover_stat] *)

val default_job_status_result : unit -> job_status_result 
(** [default_job_status_result ()] is a new empty value for type [job_status_result] *)

val default_result_entry : unit -> result_entry 
(** [default_result_entry ()] is a new empty value for type [result_entry] *)

val default_result_row : unit -> result_row 
(** [default_result_row ()] is a new empty value for type [result_row] *)

val default_job_results_result : unit -> job_results_result 
(** [default_job_results_result ()] is a new empty value for type [job_results_result] *)

val default_query_item : unit -> query_item 
(** [default_query_item ()] is a new empty value for type [query_item] *)

val default_query_job_results_result : unit -> query_job_results_result 
(** [default_query_job_results_result ()] is a new empty value for type [query_job_results_result] *)


(** {2 Make functions} *)

val make_server_capabilities : 
  ?tools:unit ->
  unit ->
  server_capabilities
(** [make_server_capabilities … ()] is a builder for type [server_capabilities] *)

val copy_server_capabilities : server_capabilities -> server_capabilities

val server_capabilities_has_tools : server_capabilities -> bool
  (** presence of field "tools" in [server_capabilities] *)

val server_capabilities_set_tools : server_capabilities -> unit -> unit
  (** set field tools in server_capabilities *)

val make_server_info : 
  ?name:string ->
  ?version:string ->
  unit ->
  server_info
(** [make_server_info … ()] is a builder for type [server_info] *)

val copy_server_info : server_info -> server_info

val server_info_has_name : server_info -> bool
  (** presence of field "name" in [server_info] *)

val server_info_set_name : server_info -> string -> unit
  (** set field name in server_info *)

val server_info_has_version : server_info -> bool
  (** presence of field "version" in [server_info] *)

val server_info_set_version : server_info -> string -> unit
  (** set field version in server_info *)

val make_initialize_result : 
  ?protocol_version:string ->
  ?capabilities:server_capabilities ->
  ?server_info:server_info ->
  unit ->
  initialize_result
(** [make_initialize_result … ()] is a builder for type [initialize_result] *)

val copy_initialize_result : initialize_result -> initialize_result

val initialize_result_has_protocol_version : initialize_result -> bool
  (** presence of field "protocol_version" in [initialize_result] *)

val initialize_result_set_protocol_version : initialize_result -> string -> unit
  (** set field protocol_version in initialize_result *)

val initialize_result_has_capabilities : initialize_result -> bool
  (** presence of field "capabilities" in [initialize_result] *)

val initialize_result_set_capabilities : initialize_result -> server_capabilities -> unit
  (** set field capabilities in initialize_result *)

val initialize_result_has_server_info : initialize_result -> bool
  (** presence of field "server_info" in [initialize_result] *)

val initialize_result_set_server_info : initialize_result -> server_info -> unit
  (** set field server_info in initialize_result *)

val make_tool_def : 
  ?name:string ->
  ?description:string ->
  ?input_schema_json:string ->
  unit ->
  tool_def
(** [make_tool_def … ()] is a builder for type [tool_def] *)

val copy_tool_def : tool_def -> tool_def

val tool_def_has_name : tool_def -> bool
  (** presence of field "name" in [tool_def] *)

val tool_def_set_name : tool_def -> string -> unit
  (** set field name in tool_def *)

val tool_def_has_description : tool_def -> bool
  (** presence of field "description" in [tool_def] *)

val tool_def_set_description : tool_def -> string -> unit
  (** set field description in tool_def *)

val tool_def_has_input_schema_json : tool_def -> bool
  (** presence of field "input_schema_json" in [tool_def] *)

val tool_def_set_input_schema_json : tool_def -> string -> unit
  (** set field input_schema_json in tool_def *)

val make_tools_list_result : 
  ?tools:tool_def list ->
  unit ->
  tools_list_result
(** [make_tools_list_result … ()] is a builder for type [tools_list_result] *)

val copy_tools_list_result : tools_list_result -> tools_list_result

val tools_list_result_set_tools : tools_list_result -> tool_def list -> unit
  (** set field tools in tools_list_result *)

val make_tool_call_result : 
  ?content_json:string ->
  unit ->
  tool_call_result
(** [make_tool_call_result … ()] is a builder for type [tool_call_result] *)

val copy_tool_call_result : tool_call_result -> tool_call_result

val tool_call_result_has_content_json : tool_call_result -> bool
  (** presence of field "content_json" in [tool_call_result] *)

val tool_call_result_set_content_json : tool_call_result -> string -> unit
  (** set field content_json in tool_call_result *)

val make_job_summary : 
  ?file:string ->
  ?uuid:string ->
  ?timestamp:float ->
  ?n_results:int32 ->
  ?n_bad:int32 ->
  ?provers:string list ->
  ?size_bytes:int32 ->
  unit ->
  job_summary
(** [make_job_summary … ()] is a builder for type [job_summary] *)

val copy_job_summary : job_summary -> job_summary

val job_summary_has_file : job_summary -> bool
  (** presence of field "file" in [job_summary] *)

val job_summary_set_file : job_summary -> string -> unit
  (** set field file in job_summary *)

val job_summary_has_uuid : job_summary -> bool
  (** presence of field "uuid" in [job_summary] *)

val job_summary_set_uuid : job_summary -> string -> unit
  (** set field uuid in job_summary *)

val job_summary_has_timestamp : job_summary -> bool
  (** presence of field "timestamp" in [job_summary] *)

val job_summary_set_timestamp : job_summary -> float -> unit
  (** set field timestamp in job_summary *)

val job_summary_has_n_results : job_summary -> bool
  (** presence of field "n_results" in [job_summary] *)

val job_summary_set_n_results : job_summary -> int32 -> unit
  (** set field n_results in job_summary *)

val job_summary_has_n_bad : job_summary -> bool
  (** presence of field "n_bad" in [job_summary] *)

val job_summary_set_n_bad : job_summary -> int32 -> unit
  (** set field n_bad in job_summary *)

val job_summary_set_provers : job_summary -> string list -> unit
  (** set field provers in job_summary *)

val job_summary_has_size_bytes : job_summary -> bool
  (** presence of field "size_bytes" in [job_summary] *)

val job_summary_set_size_bytes : job_summary -> int32 -> unit
  (** set field size_bytes in job_summary *)

val make_list_jobs_result : 
  ?jobs:job_summary list ->
  ?offset:int32 ->
  ?limit:int32 ->
  unit ->
  list_jobs_result
(** [make_list_jobs_result … ()] is a builder for type [list_jobs_result] *)

val copy_list_jobs_result : list_jobs_result -> list_jobs_result

val list_jobs_result_set_jobs : list_jobs_result -> job_summary list -> unit
  (** set field jobs in list_jobs_result *)

val list_jobs_result_has_offset : list_jobs_result -> bool
  (** presence of field "offset" in [list_jobs_result] *)

val list_jobs_result_set_offset : list_jobs_result -> int32 -> unit
  (** set field offset in list_jobs_result *)

val list_jobs_result_has_limit : list_jobs_result -> bool
  (** presence of field "limit" in [list_jobs_result] *)

val list_jobs_result_set_limit : list_jobs_result -> int32 -> unit
  (** set field limit in list_jobs_result *)

val make_prover_stat : 
  ?prover:string ->
  ?sat:int32 ->
  ?unsat:int32 ->
  ?errors:int32 ->
  ?timeout:int32 ->
  ?unknown:int32 ->
  ?memory:int32 ->
  ?total:int32 ->
  ?total_time:float ->
  ?total_time_solved:float ->
  unit ->
  prover_stat
(** [make_prover_stat … ()] is a builder for type [prover_stat] *)

val copy_prover_stat : prover_stat -> prover_stat

val prover_stat_has_prover : prover_stat -> bool
  (** presence of field "prover" in [prover_stat] *)

val prover_stat_set_prover : prover_stat -> string -> unit
  (** set field prover in prover_stat *)

val prover_stat_has_sat : prover_stat -> bool
  (** presence of field "sat" in [prover_stat] *)

val prover_stat_set_sat : prover_stat -> int32 -> unit
  (** set field sat in prover_stat *)

val prover_stat_has_unsat : prover_stat -> bool
  (** presence of field "unsat" in [prover_stat] *)

val prover_stat_set_unsat : prover_stat -> int32 -> unit
  (** set field unsat in prover_stat *)

val prover_stat_has_errors : prover_stat -> bool
  (** presence of field "errors" in [prover_stat] *)

val prover_stat_set_errors : prover_stat -> int32 -> unit
  (** set field errors in prover_stat *)

val prover_stat_has_timeout : prover_stat -> bool
  (** presence of field "timeout" in [prover_stat] *)

val prover_stat_set_timeout : prover_stat -> int32 -> unit
  (** set field timeout in prover_stat *)

val prover_stat_has_unknown : prover_stat -> bool
  (** presence of field "unknown" in [prover_stat] *)

val prover_stat_set_unknown : prover_stat -> int32 -> unit
  (** set field unknown in prover_stat *)

val prover_stat_has_memory : prover_stat -> bool
  (** presence of field "memory" in [prover_stat] *)

val prover_stat_set_memory : prover_stat -> int32 -> unit
  (** set field memory in prover_stat *)

val prover_stat_has_total : prover_stat -> bool
  (** presence of field "total" in [prover_stat] *)

val prover_stat_set_total : prover_stat -> int32 -> unit
  (** set field total in prover_stat *)

val prover_stat_has_total_time : prover_stat -> bool
  (** presence of field "total_time" in [prover_stat] *)

val prover_stat_set_total_time : prover_stat -> float -> unit
  (** set field total_time in prover_stat *)

val prover_stat_has_total_time_solved : prover_stat -> bool
  (** presence of field "total_time_solved" in [prover_stat] *)

val prover_stat_set_total_time_solved : prover_stat -> float -> unit
  (** set field total_time_solved in prover_stat *)

val make_job_status_result : 
  ?file:string ->
  ?uuid:string ->
  ?timestamp:float ->
  ?total_wall_time:float ->
  ?n_results:int32 ->
  ?n_bad:int32 ->
  ?is_complete:bool ->
  ?provers:prover_stat list ->
  unit ->
  job_status_result
(** [make_job_status_result … ()] is a builder for type [job_status_result] *)

val copy_job_status_result : job_status_result -> job_status_result

val job_status_result_has_file : job_status_result -> bool
  (** presence of field "file" in [job_status_result] *)

val job_status_result_set_file : job_status_result -> string -> unit
  (** set field file in job_status_result *)

val job_status_result_has_uuid : job_status_result -> bool
  (** presence of field "uuid" in [job_status_result] *)

val job_status_result_set_uuid : job_status_result -> string -> unit
  (** set field uuid in job_status_result *)

val job_status_result_has_timestamp : job_status_result -> bool
  (** presence of field "timestamp" in [job_status_result] *)

val job_status_result_set_timestamp : job_status_result -> float -> unit
  (** set field timestamp in job_status_result *)

val job_status_result_has_total_wall_time : job_status_result -> bool
  (** presence of field "total_wall_time" in [job_status_result] *)

val job_status_result_set_total_wall_time : job_status_result -> float -> unit
  (** set field total_wall_time in job_status_result *)

val job_status_result_has_n_results : job_status_result -> bool
  (** presence of field "n_results" in [job_status_result] *)

val job_status_result_set_n_results : job_status_result -> int32 -> unit
  (** set field n_results in job_status_result *)

val job_status_result_has_n_bad : job_status_result -> bool
  (** presence of field "n_bad" in [job_status_result] *)

val job_status_result_set_n_bad : job_status_result -> int32 -> unit
  (** set field n_bad in job_status_result *)

val job_status_result_has_is_complete : job_status_result -> bool
  (** presence of field "is_complete" in [job_status_result] *)

val job_status_result_set_is_complete : job_status_result -> bool -> unit
  (** set field is_complete in job_status_result *)

val job_status_result_set_provers : job_status_result -> prover_stat list -> unit
  (** set field provers in job_status_result *)

val make_result_entry : 
  ?prover:string ->
  ?result:string ->
  ?time_s:float ->
  unit ->
  result_entry
(** [make_result_entry … ()] is a builder for type [result_entry] *)

val copy_result_entry : result_entry -> result_entry

val result_entry_has_prover : result_entry -> bool
  (** presence of field "prover" in [result_entry] *)

val result_entry_set_prover : result_entry -> string -> unit
  (** set field prover in result_entry *)

val result_entry_has_result : result_entry -> bool
  (** presence of field "result" in [result_entry] *)

val result_entry_set_result : result_entry -> string -> unit
  (** set field result in result_entry *)

val result_entry_has_time_s : result_entry -> bool
  (** presence of field "time_s" in [result_entry] *)

val result_entry_set_time_s : result_entry -> float -> unit
  (** set field time_s in result_entry *)

val make_result_row : 
  ?problem:string ->
  ?results:result_entry list ->
  unit ->
  result_row
(** [make_result_row … ()] is a builder for type [result_row] *)

val copy_result_row : result_row -> result_row

val result_row_has_problem : result_row -> bool
  (** presence of field "problem" in [result_row] *)

val result_row_set_problem : result_row -> string -> unit
  (** set field problem in result_row *)

val result_row_set_results : result_row -> result_entry list -> unit
  (** set field results in result_row *)

val make_job_results_result : 
  ?file:string ->
  ?provers:string list ->
  ?rows:result_row list ->
  ?n_rows:int32 ->
  unit ->
  job_results_result
(** [make_job_results_result … ()] is a builder for type [job_results_result] *)

val copy_job_results_result : job_results_result -> job_results_result

val job_results_result_has_file : job_results_result -> bool
  (** presence of field "file" in [job_results_result] *)

val job_results_result_set_file : job_results_result -> string -> unit
  (** set field file in job_results_result *)

val job_results_result_set_provers : job_results_result -> string list -> unit
  (** set field provers in job_results_result *)

val job_results_result_set_rows : job_results_result -> result_row list -> unit
  (** set field rows in job_results_result *)

val job_results_result_has_n_rows : job_results_result -> bool
  (** presence of field "n_rows" in [job_results_result] *)

val job_results_result_set_n_rows : job_results_result -> int32 -> unit
  (** set field n_rows in job_results_result *)

val make_query_item : 
  ?prover:string ->
  ?problem:string ->
  ?result:string ->
  ?expected:string ->
  ?time_s:float ->
  unit ->
  query_item
(** [make_query_item … ()] is a builder for type [query_item] *)

val copy_query_item : query_item -> query_item

val query_item_has_prover : query_item -> bool
  (** presence of field "prover" in [query_item] *)

val query_item_set_prover : query_item -> string -> unit
  (** set field prover in query_item *)

val query_item_has_problem : query_item -> bool
  (** presence of field "problem" in [query_item] *)

val query_item_set_problem : query_item -> string -> unit
  (** set field problem in query_item *)

val query_item_has_result : query_item -> bool
  (** presence of field "result" in [query_item] *)

val query_item_set_result : query_item -> string -> unit
  (** set field result in query_item *)

val query_item_has_expected : query_item -> bool
  (** presence of field "expected" in [query_item] *)

val query_item_set_expected : query_item -> string -> unit
  (** set field expected in query_item *)

val query_item_has_time_s : query_item -> bool
  (** presence of field "time_s" in [query_item] *)

val query_item_set_time_s : query_item -> float -> unit
  (** set field time_s in query_item *)

val make_query_job_results_result : 
  ?file:string ->
  ?total:int32 ->
  ?page:int32 ->
  ?per_page:int32 ->
  ?is_complete:bool ->
  ?items:query_item list ->
  unit ->
  query_job_results_result
(** [make_query_job_results_result … ()] is a builder for type [query_job_results_result] *)

val copy_query_job_results_result : query_job_results_result -> query_job_results_result

val query_job_results_result_has_file : query_job_results_result -> bool
  (** presence of field "file" in [query_job_results_result] *)

val query_job_results_result_set_file : query_job_results_result -> string -> unit
  (** set field file in query_job_results_result *)

val query_job_results_result_has_total : query_job_results_result -> bool
  (** presence of field "total" in [query_job_results_result] *)

val query_job_results_result_set_total : query_job_results_result -> int32 -> unit
  (** set field total in query_job_results_result *)

val query_job_results_result_has_page : query_job_results_result -> bool
  (** presence of field "page" in [query_job_results_result] *)

val query_job_results_result_set_page : query_job_results_result -> int32 -> unit
  (** set field page in query_job_results_result *)

val query_job_results_result_has_per_page : query_job_results_result -> bool
  (** presence of field "per_page" in [query_job_results_result] *)

val query_job_results_result_set_per_page : query_job_results_result -> int32 -> unit
  (** set field per_page in query_job_results_result *)

val query_job_results_result_has_is_complete : query_job_results_result -> bool
  (** presence of field "is_complete" in [query_job_results_result] *)

val query_job_results_result_set_is_complete : query_job_results_result -> bool -> unit
  (** set field is_complete in query_job_results_result *)

val query_job_results_result_set_items : query_job_results_result -> query_item list -> unit
  (** set field items in query_job_results_result *)


(** {2 Protobuf YoJson Encoding} *)

val encode_json_tools_capability : tools_capability -> Yojson.Basic.t
(** [encode_json_tools_capability v encoder] encodes [v] to to json *)

val encode_json_server_capabilities : server_capabilities -> Yojson.Basic.t
(** [encode_json_server_capabilities v encoder] encodes [v] to to json *)

val encode_json_server_info : server_info -> Yojson.Basic.t
(** [encode_json_server_info v encoder] encodes [v] to to json *)

val encode_json_initialize_result : initialize_result -> Yojson.Basic.t
(** [encode_json_initialize_result v encoder] encodes [v] to to json *)

val encode_json_tool_def : tool_def -> Yojson.Basic.t
(** [encode_json_tool_def v encoder] encodes [v] to to json *)

val encode_json_tools_list_result : tools_list_result -> Yojson.Basic.t
(** [encode_json_tools_list_result v encoder] encodes [v] to to json *)

val encode_json_tool_call_result : tool_call_result -> Yojson.Basic.t
(** [encode_json_tool_call_result v encoder] encodes [v] to to json *)

val encode_json_job_summary : job_summary -> Yojson.Basic.t
(** [encode_json_job_summary v encoder] encodes [v] to to json *)

val encode_json_list_jobs_result : list_jobs_result -> Yojson.Basic.t
(** [encode_json_list_jobs_result v encoder] encodes [v] to to json *)

val encode_json_prover_stat : prover_stat -> Yojson.Basic.t
(** [encode_json_prover_stat v encoder] encodes [v] to to json *)

val encode_json_job_status_result : job_status_result -> Yojson.Basic.t
(** [encode_json_job_status_result v encoder] encodes [v] to to json *)

val encode_json_result_entry : result_entry -> Yojson.Basic.t
(** [encode_json_result_entry v encoder] encodes [v] to to json *)

val encode_json_result_row : result_row -> Yojson.Basic.t
(** [encode_json_result_row v encoder] encodes [v] to to json *)

val encode_json_job_results_result : job_results_result -> Yojson.Basic.t
(** [encode_json_job_results_result v encoder] encodes [v] to to json *)

val encode_json_query_item : query_item -> Yojson.Basic.t
(** [encode_json_query_item v encoder] encodes [v] to to json *)

val encode_json_query_job_results_result : query_job_results_result -> Yojson.Basic.t
(** [encode_json_query_job_results_result v encoder] encodes [v] to to json *)


(** {2 JSON Decoding} *)

val decode_json_tools_capability : Yojson.Basic.t -> tools_capability
(** [decode_json_tools_capability decoder] decodes a [tools_capability] value from [decoder] *)

val decode_json_server_capabilities : Yojson.Basic.t -> server_capabilities
(** [decode_json_server_capabilities decoder] decodes a [server_capabilities] value from [decoder] *)

val decode_json_server_info : Yojson.Basic.t -> server_info
(** [decode_json_server_info decoder] decodes a [server_info] value from [decoder] *)

val decode_json_initialize_result : Yojson.Basic.t -> initialize_result
(** [decode_json_initialize_result decoder] decodes a [initialize_result] value from [decoder] *)

val decode_json_tool_def : Yojson.Basic.t -> tool_def
(** [decode_json_tool_def decoder] decodes a [tool_def] value from [decoder] *)

val decode_json_tools_list_result : Yojson.Basic.t -> tools_list_result
(** [decode_json_tools_list_result decoder] decodes a [tools_list_result] value from [decoder] *)

val decode_json_tool_call_result : Yojson.Basic.t -> tool_call_result
(** [decode_json_tool_call_result decoder] decodes a [tool_call_result] value from [decoder] *)

val decode_json_job_summary : Yojson.Basic.t -> job_summary
(** [decode_json_job_summary decoder] decodes a [job_summary] value from [decoder] *)

val decode_json_list_jobs_result : Yojson.Basic.t -> list_jobs_result
(** [decode_json_list_jobs_result decoder] decodes a [list_jobs_result] value from [decoder] *)

val decode_json_prover_stat : Yojson.Basic.t -> prover_stat
(** [decode_json_prover_stat decoder] decodes a [prover_stat] value from [decoder] *)

val decode_json_job_status_result : Yojson.Basic.t -> job_status_result
(** [decode_json_job_status_result decoder] decodes a [job_status_result] value from [decoder] *)

val decode_json_result_entry : Yojson.Basic.t -> result_entry
(** [decode_json_result_entry decoder] decodes a [result_entry] value from [decoder] *)

val decode_json_result_row : Yojson.Basic.t -> result_row
(** [decode_json_result_row decoder] decodes a [result_row] value from [decoder] *)

val decode_json_job_results_result : Yojson.Basic.t -> job_results_result
(** [decode_json_job_results_result decoder] decodes a [job_results_result] value from [decoder] *)

val decode_json_query_item : Yojson.Basic.t -> query_item
(** [decode_json_query_item decoder] decodes a [query_item] value from [decoder] *)

val decode_json_query_job_results_result : Yojson.Basic.t -> query_job_results_result
(** [decode_json_query_job_results_result decoder] decodes a [query_job_results_result] value from [decoder] *)

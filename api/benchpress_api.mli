
(** Code for benchpress_api.proto *)

(* generated from "benchpress_api.proto", do not edit *)



(** {2 Types} *)

type new_job_request = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 3 fields *)
  mutable provers : string list;
  mutable paths : string list;
  mutable timeout_s : int32;
  mutable memory_mb : int32;
  mutable output_name : string;
}

type new_job_response = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable job_id : string;
}

type get_job_status_request = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable job_id : string;
}

type job_status =
  | Queued 
  | Running 
  | Completed 
  | Cancelled 
  | Failed 

type get_job_status_response = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 4 fields *)
  mutable job_id : string;
  mutable status : job_status;
  mutable progress_percent : int32;
  mutable result_file : string;
}

type cancel_job_request = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable job_id : string;
}

type cancel_job_response = unit


(** {2 Basic values} *)

val default_new_job_request : unit -> new_job_request 
(** [default_new_job_request ()] is a new empty value for type [new_job_request] *)

val default_new_job_response : unit -> new_job_response 
(** [default_new_job_response ()] is a new empty value for type [new_job_response] *)

val default_get_job_status_request : unit -> get_job_status_request 
(** [default_get_job_status_request ()] is a new empty value for type [get_job_status_request] *)

val default_job_status : unit -> job_status
(** [default_job_status ()] is a new empty value for type [job_status] *)

val default_get_job_status_response : unit -> get_job_status_response 
(** [default_get_job_status_response ()] is a new empty value for type [get_job_status_response] *)

val default_cancel_job_request : unit -> cancel_job_request 
(** [default_cancel_job_request ()] is a new empty value for type [cancel_job_request] *)

val default_cancel_job_response : unit
(** [default_cancel_job_response] is the default value for type [cancel_job_response] *)


(** {2 Make functions} *)

val make_new_job_request : 
  ?provers:string list ->
  ?paths:string list ->
  ?timeout_s:int32 ->
  ?memory_mb:int32 ->
  ?output_name:string ->
  unit ->
  new_job_request
(** [make_new_job_request … ()] is a builder for type [new_job_request] *)

val copy_new_job_request : new_job_request -> new_job_request

val new_job_request_set_provers : new_job_request -> string list -> unit
  (** set field provers in new_job_request *)

val new_job_request_set_paths : new_job_request -> string list -> unit
  (** set field paths in new_job_request *)

val new_job_request_has_timeout_s : new_job_request -> bool
  (** presence of field "timeout_s" in [new_job_request] *)

val new_job_request_set_timeout_s : new_job_request -> int32 -> unit
  (** set field timeout_s in new_job_request *)

val new_job_request_has_memory_mb : new_job_request -> bool
  (** presence of field "memory_mb" in [new_job_request] *)

val new_job_request_set_memory_mb : new_job_request -> int32 -> unit
  (** set field memory_mb in new_job_request *)

val new_job_request_has_output_name : new_job_request -> bool
  (** presence of field "output_name" in [new_job_request] *)

val new_job_request_set_output_name : new_job_request -> string -> unit
  (** set field output_name in new_job_request *)

val make_new_job_response : 
  ?job_id:string ->
  unit ->
  new_job_response
(** [make_new_job_response … ()] is a builder for type [new_job_response] *)

val copy_new_job_response : new_job_response -> new_job_response

val new_job_response_has_job_id : new_job_response -> bool
  (** presence of field "job_id" in [new_job_response] *)

val new_job_response_set_job_id : new_job_response -> string -> unit
  (** set field job_id in new_job_response *)

val make_get_job_status_request : 
  ?job_id:string ->
  unit ->
  get_job_status_request
(** [make_get_job_status_request … ()] is a builder for type [get_job_status_request] *)

val copy_get_job_status_request : get_job_status_request -> get_job_status_request

val get_job_status_request_has_job_id : get_job_status_request -> bool
  (** presence of field "job_id" in [get_job_status_request] *)

val get_job_status_request_set_job_id : get_job_status_request -> string -> unit
  (** set field job_id in get_job_status_request *)

val make_get_job_status_response : 
  ?job_id:string ->
  ?status:job_status ->
  ?progress_percent:int32 ->
  ?result_file:string ->
  unit ->
  get_job_status_response
(** [make_get_job_status_response … ()] is a builder for type [get_job_status_response] *)

val copy_get_job_status_response : get_job_status_response -> get_job_status_response

val get_job_status_response_has_job_id : get_job_status_response -> bool
  (** presence of field "job_id" in [get_job_status_response] *)

val get_job_status_response_set_job_id : get_job_status_response -> string -> unit
  (** set field job_id in get_job_status_response *)

val get_job_status_response_has_status : get_job_status_response -> bool
  (** presence of field "status" in [get_job_status_response] *)

val get_job_status_response_set_status : get_job_status_response -> job_status -> unit
  (** set field status in get_job_status_response *)

val get_job_status_response_has_progress_percent : get_job_status_response -> bool
  (** presence of field "progress_percent" in [get_job_status_response] *)

val get_job_status_response_set_progress_percent : get_job_status_response -> int32 -> unit
  (** set field progress_percent in get_job_status_response *)

val get_job_status_response_has_result_file : get_job_status_response -> bool
  (** presence of field "result_file" in [get_job_status_response] *)

val get_job_status_response_set_result_file : get_job_status_response -> string -> unit
  (** set field result_file in get_job_status_response *)

val make_cancel_job_request : 
  ?job_id:string ->
  unit ->
  cancel_job_request
(** [make_cancel_job_request … ()] is a builder for type [cancel_job_request] *)

val copy_cancel_job_request : cancel_job_request -> cancel_job_request

val cancel_job_request_has_job_id : cancel_job_request -> bool
  (** presence of field "job_id" in [cancel_job_request] *)

val cancel_job_request_set_job_id : cancel_job_request -> string -> unit
  (** set field job_id in cancel_job_request *)


(** {2 Formatters} *)

val pp_new_job_request : Format.formatter -> new_job_request -> unit 
(** [pp_new_job_request v] formats v *)

val pp_new_job_response : Format.formatter -> new_job_response -> unit 
(** [pp_new_job_response v] formats v *)

val pp_get_job_status_request : Format.formatter -> get_job_status_request -> unit 
(** [pp_get_job_status_request v] formats v *)

val pp_job_status : Format.formatter -> job_status -> unit 
(** [pp_job_status v] formats v *)

val pp_get_job_status_response : Format.formatter -> get_job_status_response -> unit 
(** [pp_get_job_status_response v] formats v *)

val pp_cancel_job_request : Format.formatter -> cancel_job_request -> unit 
(** [pp_cancel_job_request v] formats v *)

val pp_cancel_job_response : Format.formatter -> cancel_job_response -> unit 
(** [pp_cancel_job_response v] formats v *)


(** {2 Protobuf Encoding} *)

val encode_pb_new_job_request : new_job_request -> Pbrt.Encoder.t -> unit
(** [encode_pb_new_job_request v encoder] encodes [v] with the given [encoder] *)

val encode_pb_new_job_response : new_job_response -> Pbrt.Encoder.t -> unit
(** [encode_pb_new_job_response v encoder] encodes [v] with the given [encoder] *)

val encode_pb_get_job_status_request : get_job_status_request -> Pbrt.Encoder.t -> unit
(** [encode_pb_get_job_status_request v encoder] encodes [v] with the given [encoder] *)

val encode_pb_job_status : job_status -> Pbrt.Encoder.t -> unit
(** [encode_pb_job_status v encoder] encodes [v] with the given [encoder] *)

val encode_pb_get_job_status_response : get_job_status_response -> Pbrt.Encoder.t -> unit
(** [encode_pb_get_job_status_response v encoder] encodes [v] with the given [encoder] *)

val encode_pb_cancel_job_request : cancel_job_request -> Pbrt.Encoder.t -> unit
(** [encode_pb_cancel_job_request v encoder] encodes [v] with the given [encoder] *)

val encode_pb_cancel_job_response : cancel_job_response -> Pbrt.Encoder.t -> unit
(** [encode_pb_cancel_job_response v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_new_job_request : Pbrt.Decoder.t -> new_job_request
(** [decode_pb_new_job_request decoder] decodes a [new_job_request] binary value from [decoder] *)

val decode_pb_new_job_response : Pbrt.Decoder.t -> new_job_response
(** [decode_pb_new_job_response decoder] decodes a [new_job_response] binary value from [decoder] *)

val decode_pb_get_job_status_request : Pbrt.Decoder.t -> get_job_status_request
(** [decode_pb_get_job_status_request decoder] decodes a [get_job_status_request] binary value from [decoder] *)

val decode_pb_job_status : Pbrt.Decoder.t -> job_status
(** [decode_pb_job_status decoder] decodes a [job_status] binary value from [decoder] *)

val decode_pb_get_job_status_response : Pbrt.Decoder.t -> get_job_status_response
(** [decode_pb_get_job_status_response decoder] decodes a [get_job_status_response] binary value from [decoder] *)

val decode_pb_cancel_job_request : Pbrt.Decoder.t -> cancel_job_request
(** [decode_pb_cancel_job_request decoder] decodes a [cancel_job_request] binary value from [decoder] *)

val decode_pb_cancel_job_response : Pbrt.Decoder.t -> cancel_job_response
(** [decode_pb_cancel_job_response decoder] decodes a [cancel_job_response] binary value from [decoder] *)


(** {2 Protobuf YoJson Encoding} *)

val encode_json_new_job_request : new_job_request -> Yojson.Basic.t
(** [encode_json_new_job_request v encoder] encodes [v] to to json *)

val encode_json_new_job_response : new_job_response -> Yojson.Basic.t
(** [encode_json_new_job_response v encoder] encodes [v] to to json *)

val encode_json_get_job_status_request : get_job_status_request -> Yojson.Basic.t
(** [encode_json_get_job_status_request v encoder] encodes [v] to to json *)

val encode_json_job_status : job_status -> Yojson.Basic.t
(** [encode_json_job_status v encoder] encodes [v] to to json *)

val encode_json_get_job_status_response : get_job_status_response -> Yojson.Basic.t
(** [encode_json_get_job_status_response v encoder] encodes [v] to to json *)

val encode_json_cancel_job_request : cancel_job_request -> Yojson.Basic.t
(** [encode_json_cancel_job_request v encoder] encodes [v] to to json *)

val encode_json_cancel_job_response : cancel_job_response -> Yojson.Basic.t
(** [encode_json_cancel_job_response v encoder] encodes [v] to to json *)


(** {2 JSON Decoding} *)

val decode_json_new_job_request : Yojson.Basic.t -> new_job_request
(** [decode_json_new_job_request decoder] decodes a [new_job_request] value from [decoder] *)

val decode_json_new_job_response : Yojson.Basic.t -> new_job_response
(** [decode_json_new_job_response decoder] decodes a [new_job_response] value from [decoder] *)

val decode_json_get_job_status_request : Yojson.Basic.t -> get_job_status_request
(** [decode_json_get_job_status_request decoder] decodes a [get_job_status_request] value from [decoder] *)

val decode_json_job_status : Yojson.Basic.t -> job_status
(** [decode_json_job_status decoder] decodes a [job_status] value from [decoder] *)

val decode_json_get_job_status_response : Yojson.Basic.t -> get_job_status_response
(** [decode_json_get_job_status_response decoder] decodes a [get_job_status_response] value from [decoder] *)

val decode_json_cancel_job_request : Yojson.Basic.t -> cancel_job_request
(** [decode_json_cancel_job_request decoder] decodes a [cancel_job_request] value from [decoder] *)

val decode_json_cancel_job_response : Yojson.Basic.t -> cancel_job_response
(** [decode_json_cancel_job_response decoder] decodes a [cancel_job_response] value from [decoder] *)


(** {2 Services} *)

(** BenchpressApi service *)
module BenchpressApi : sig
  open Pbrt_services
  open Pbrt_services.Value_mode
  
  module Client : sig
    
    val newJob : (new_job_request, unary, new_job_response, unary) Client.rpc
    
    val getJobStatus : (get_job_status_request, unary, get_job_status_response, unary) Client.rpc
    
    val cancelJob : (cancel_job_request, unary, cancel_job_response, unary) Client.rpc
  end
  
  module Server : sig
    (** Produce a server implementation from handlers *)
    val make : 
      newJob:((new_job_request, unary, new_job_response, unary) Server.rpc -> 'handler) ->
      getJobStatus:((get_job_status_request, unary, get_job_status_response, unary) Server.rpc -> 'handler) ->
      cancelJob:((cancel_job_request, unary, cancel_job_response, unary) Server.rpc -> 'handler) ->
      unit -> 'handler Pbrt_services.Server.t
    
    (** The individual server stubs are only exposed for advanced users. Casual users should prefer accessing them through {!make}. *)
    
    val newJob : (new_job_request,unary,new_job_response,unary) Server.rpc
    
    val getJobStatus : (get_job_status_request,unary,get_job_status_response,unary) Server.rpc
    
    val cancelJob : (cancel_job_request,unary,cancel_job_response,unary) Server.rpc
  end
end

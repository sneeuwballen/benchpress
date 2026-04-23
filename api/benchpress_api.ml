[@@@ocaml.warning "-23-27-30-39-44"]

type new_job_request = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 3 fields *)
  mutable provers : string list;
  mutable paths : string list;
  mutable timeout_s : int32;
  mutable memory_mb : int32;
  mutable output_name : string;
}

type new_job_response = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable job_id : string;
}

type get_job_status_request = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable job_id : string;
}

type job_status =
  | Queued 
  | Running 
  | Completed 
  | Cancelled 
  | Failed 

type get_job_status_response = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 4 fields *)
  mutable job_id : string;
  mutable status : job_status;
  mutable progress_percent : int32;
  mutable result_file : string;
}

type cancel_job_request = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable job_id : string;
}

type cancel_job_response = unit

let default_new_job_request (): new_job_request =
{
  _presence=Pbrt.Bitfield.empty;
  provers=[];
  paths=[];
  timeout_s=0l;
  memory_mb=0l;
  output_name="";
}

let default_new_job_response (): new_job_response =
{
  _presence=Pbrt.Bitfield.empty;
  job_id="";
}

let default_get_job_status_request (): get_job_status_request =
{
  _presence=Pbrt.Bitfield.empty;
  job_id="";
}

let default_job_status () = (Queued:job_status)

let default_get_job_status_response (): get_job_status_response =
{
  _presence=Pbrt.Bitfield.empty;
  job_id="";
  status=default_job_status ();
  progress_percent=0l;
  result_file="";
}

let default_cancel_job_request (): cancel_job_request =
{
  _presence=Pbrt.Bitfield.empty;
  job_id="";
}

let default_cancel_job_response : cancel_job_response = ()


(** {2 Make functions} *)

let[@inline] new_job_request_has_timeout_s (self:new_job_request) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] new_job_request_has_memory_mb (self:new_job_request) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] new_job_request_has_output_name (self:new_job_request) : bool = (Pbrt.Bitfield.get self._presence 2)

let[@inline] new_job_request_set_provers (self:new_job_request) (x:string list) : unit =
  self.provers <- x
let[@inline] new_job_request_set_paths (self:new_job_request) (x:string list) : unit =
  self.paths <- x
let[@inline] new_job_request_set_timeout_s (self:new_job_request) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.timeout_s <- x
let[@inline] new_job_request_set_memory_mb (self:new_job_request) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.memory_mb <- x
let[@inline] new_job_request_set_output_name (self:new_job_request) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.output_name <- x

let copy_new_job_request (self:new_job_request) : new_job_request =
  { self with provers = self.provers }

let make_new_job_request 
  ?(provers=[])
  ?(paths=[])
  ?(timeout_s:int32 option)
  ?(memory_mb:int32 option)
  ?(output_name:string option)
  () : new_job_request  =
  let _res = default_new_job_request () in
  new_job_request_set_provers _res provers;
  new_job_request_set_paths _res paths;
  (match timeout_s with
  | None -> ()
  | Some v -> new_job_request_set_timeout_s _res v);
  (match memory_mb with
  | None -> ()
  | Some v -> new_job_request_set_memory_mb _res v);
  (match output_name with
  | None -> ()
  | Some v -> new_job_request_set_output_name _res v);
  _res

let[@inline] new_job_response_has_job_id (self:new_job_response) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] new_job_response_set_job_id (self:new_job_response) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.job_id <- x

let copy_new_job_response (self:new_job_response) : new_job_response =
  { self with job_id = self.job_id }

let make_new_job_response 
  ?(job_id:string option)
  () : new_job_response  =
  let _res = default_new_job_response () in
  (match job_id with
  | None -> ()
  | Some v -> new_job_response_set_job_id _res v);
  _res

let[@inline] get_job_status_request_has_job_id (self:get_job_status_request) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] get_job_status_request_set_job_id (self:get_job_status_request) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.job_id <- x

let copy_get_job_status_request (self:get_job_status_request) : get_job_status_request =
  { self with job_id = self.job_id }

let make_get_job_status_request 
  ?(job_id:string option)
  () : get_job_status_request  =
  let _res = default_get_job_status_request () in
  (match job_id with
  | None -> ()
  | Some v -> get_job_status_request_set_job_id _res v);
  _res

let[@inline] get_job_status_response_has_job_id (self:get_job_status_response) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] get_job_status_response_has_status (self:get_job_status_response) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] get_job_status_response_has_progress_percent (self:get_job_status_response) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] get_job_status_response_has_result_file (self:get_job_status_response) : bool = (Pbrt.Bitfield.get self._presence 3)

let[@inline] get_job_status_response_set_job_id (self:get_job_status_response) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.job_id <- x
let[@inline] get_job_status_response_set_status (self:get_job_status_response) (x:job_status) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.status <- x
let[@inline] get_job_status_response_set_progress_percent (self:get_job_status_response) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.progress_percent <- x
let[@inline] get_job_status_response_set_result_file (self:get_job_status_response) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.result_file <- x

let copy_get_job_status_response (self:get_job_status_response) : get_job_status_response =
  { self with job_id = self.job_id }

let make_get_job_status_response 
  ?(job_id:string option)
  ?(status:job_status option)
  ?(progress_percent:int32 option)
  ?(result_file:string option)
  () : get_job_status_response  =
  let _res = default_get_job_status_response () in
  (match job_id with
  | None -> ()
  | Some v -> get_job_status_response_set_job_id _res v);
  (match status with
  | None -> ()
  | Some v -> get_job_status_response_set_status _res v);
  (match progress_percent with
  | None -> ()
  | Some v -> get_job_status_response_set_progress_percent _res v);
  (match result_file with
  | None -> ()
  | Some v -> get_job_status_response_set_result_file _res v);
  _res

let[@inline] cancel_job_request_has_job_id (self:cancel_job_request) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] cancel_job_request_set_job_id (self:cancel_job_request) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.job_id <- x

let copy_cancel_job_request (self:cancel_job_request) : cancel_job_request =
  { self with job_id = self.job_id }

let make_cancel_job_request 
  ?(job_id:string option)
  () : cancel_job_request  =
  let _res = default_cancel_job_request () in
  (match job_id with
  | None -> ()
  | Some v -> cancel_job_request_set_job_id _res v);
  _res

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Formatters} *)

let rec pp_new_job_request fmt (v:new_job_request) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "provers" (Pbrt.Pp.pp_list Pbrt.Pp.pp_string) fmt v.provers;
    Pbrt.Pp.pp_record_field ~first:false "paths" (Pbrt.Pp.pp_list Pbrt.Pp.pp_string) fmt v.paths;
    Pbrt.Pp.pp_record_field ~absent:(not (new_job_request_has_timeout_s v)) ~first:false "timeout_s" Pbrt.Pp.pp_int32 fmt v.timeout_s;
    Pbrt.Pp.pp_record_field ~absent:(not (new_job_request_has_memory_mb v)) ~first:false "memory_mb" Pbrt.Pp.pp_int32 fmt v.memory_mb;
    Pbrt.Pp.pp_record_field ~absent:(not (new_job_request_has_output_name v)) ~first:false "output_name" Pbrt.Pp.pp_string fmt v.output_name;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_new_job_response fmt (v:new_job_response) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~absent:(not (new_job_response_has_job_id v)) ~first:true "job_id" Pbrt.Pp.pp_string fmt v.job_id;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_get_job_status_request fmt (v:get_job_status_request) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~absent:(not (get_job_status_request_has_job_id v)) ~first:true "job_id" Pbrt.Pp.pp_string fmt v.job_id;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_job_status fmt (v:job_status) =
  match v with
  | Queued -> Format.fprintf fmt "Queued"
  | Running -> Format.fprintf fmt "Running"
  | Completed -> Format.fprintf fmt "Completed"
  | Cancelled -> Format.fprintf fmt "Cancelled"
  | Failed -> Format.fprintf fmt "Failed"

let rec pp_get_job_status_response fmt (v:get_job_status_response) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~absent:(not (get_job_status_response_has_job_id v)) ~first:true "job_id" Pbrt.Pp.pp_string fmt v.job_id;
    Pbrt.Pp.pp_record_field ~absent:(not (get_job_status_response_has_status v)) ~first:false "status" pp_job_status fmt v.status;
    Pbrt.Pp.pp_record_field ~absent:(not (get_job_status_response_has_progress_percent v)) ~first:false "progress_percent" Pbrt.Pp.pp_int32 fmt v.progress_percent;
    Pbrt.Pp.pp_record_field ~absent:(not (get_job_status_response_has_result_file v)) ~first:false "result_file" Pbrt.Pp.pp_string fmt v.result_file;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_cancel_job_request fmt (v:cancel_job_request) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~absent:(not (cancel_job_request_has_job_id v)) ~first:true "job_id" Pbrt.Pp.pp_string fmt v.job_id;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_cancel_job_response fmt (v:cancel_job_response) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_unit fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_new_job_request (v:new_job_request) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.provers encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.paths encoder;
  if new_job_request_has_timeout_s v then (
    Pbrt.Encoder.int32_as_varint v.timeout_s encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  );
  if new_job_request_has_memory_mb v then (
    Pbrt.Encoder.int32_as_varint v.memory_mb encoder;
    Pbrt.Encoder.key 4 Pbrt.Varint encoder; 
  );
  if new_job_request_has_output_name v then (
    Pbrt.Encoder.string v.output_name encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  );
  ()

let rec encode_pb_new_job_response (v:new_job_response) encoder = 
  if new_job_response_has_job_id v then (
    Pbrt.Encoder.string v.job_id encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  ()

let rec encode_pb_get_job_status_request (v:get_job_status_request) encoder = 
  if get_job_status_request_has_job_id v then (
    Pbrt.Encoder.string v.job_id encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  ()

let rec encode_pb_job_status (v:job_status) encoder =
  match v with
  | Queued -> Pbrt.Encoder.int_as_varint (0) encoder
  | Running -> Pbrt.Encoder.int_as_varint 1 encoder
  | Completed -> Pbrt.Encoder.int_as_varint 2 encoder
  | Cancelled -> Pbrt.Encoder.int_as_varint 3 encoder
  | Failed -> Pbrt.Encoder.int_as_varint 4 encoder

let rec encode_pb_get_job_status_response (v:get_job_status_response) encoder = 
  if get_job_status_response_has_job_id v then (
    Pbrt.Encoder.string v.job_id encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  if get_job_status_response_has_status v then (
    encode_pb_job_status v.status encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  );
  if get_job_status_response_has_progress_percent v then (
    Pbrt.Encoder.int32_as_varint v.progress_percent encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  );
  if get_job_status_response_has_result_file v then (
    Pbrt.Encoder.string v.result_file encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  );
  ()

let rec encode_pb_cancel_job_request (v:cancel_job_request) encoder = 
  if cancel_job_request_has_job_id v then (
    Pbrt.Encoder.string v.job_id encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  ()

let rec encode_pb_cancel_job_response (v:cancel_job_response) encoder = 
()

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_new_job_request d =
  let v = default_new_job_request () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      new_job_request_set_paths v (List.rev v.paths);
      new_job_request_set_provers v (List.rev v.provers);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      new_job_request_set_provers v ((Pbrt.Decoder.string d) :: v.provers);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "new_job_request" 1 pk
    | Some (2, Pbrt.Bytes) -> begin
      new_job_request_set_paths v ((Pbrt.Decoder.string d) :: v.paths);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "new_job_request" 2 pk
    | Some (3, Pbrt.Varint) -> begin
      new_job_request_set_timeout_s v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "new_job_request" 3 pk
    | Some (4, Pbrt.Varint) -> begin
      new_job_request_set_memory_mb v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "new_job_request" 4 pk
    | Some (5, Pbrt.Bytes) -> begin
      new_job_request_set_output_name v (Pbrt.Decoder.string d);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "new_job_request" 5 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : new_job_request)

let rec decode_pb_new_job_response d =
  let v = default_new_job_response () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      new_job_response_set_job_id v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "new_job_response" 1 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : new_job_response)

let rec decode_pb_get_job_status_request d =
  let v = default_get_job_status_request () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      get_job_status_request_set_job_id v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "get_job_status_request" 1 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : get_job_status_request)

let rec decode_pb_job_status d : job_status = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> Queued
  | 1 -> Running
  | 2 -> Completed
  | 3 -> Cancelled
  | 4 -> Failed
  | _ -> Pbrt.Decoder.malformed_variant "job_status"

let rec decode_pb_get_job_status_response d =
  let v = default_get_job_status_response () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      get_job_status_response_set_job_id v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "get_job_status_response" 1 pk
    | Some (2, Pbrt.Varint) -> begin
      get_job_status_response_set_status v (decode_pb_job_status d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "get_job_status_response" 2 pk
    | Some (3, Pbrt.Varint) -> begin
      get_job_status_response_set_progress_percent v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "get_job_status_response" 3 pk
    | Some (4, Pbrt.Bytes) -> begin
      get_job_status_response_set_result_file v (Pbrt.Decoder.string d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "get_job_status_response" 4 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : get_job_status_response)

let rec decode_pb_cancel_job_request d =
  let v = default_cancel_job_request () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      cancel_job_request_set_job_id v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "cancel_job_request" 1 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : cancel_job_request)

let rec decode_pb_cancel_job_response d =
  match Pbrt.Decoder.key d with
  | None -> ();
  | Some (_, pk) -> 
    Pbrt.Decoder.unexpected_payload "Unexpected fields in empty message(cancel_job_response)" pk

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf YoJson Encoding} *)

let rec encode_json_new_job_request (v:new_job_request) = 
  let assoc = ref [] in
  assoc := (
    let l = v.provers |> List.map Pbrt_yojson.make_string in
    ("provers", `List l) :: !assoc 
  );
  assoc := (
    let l = v.paths |> List.map Pbrt_yojson.make_string in
    ("paths", `List l) :: !assoc 
  );
  if new_job_request_has_timeout_s v then (
    assoc := ("timeoutS", Pbrt_yojson.make_int (Int32.to_int v.timeout_s)) :: !assoc;
  );
  if new_job_request_has_memory_mb v then (
    assoc := ("memoryMb", Pbrt_yojson.make_int (Int32.to_int v.memory_mb)) :: !assoc;
  );
  if new_job_request_has_output_name v then (
    assoc := ("outputName", Pbrt_yojson.make_string v.output_name) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_new_job_response (v:new_job_response) = 
  let assoc = ref [] in
  if new_job_response_has_job_id v then (
    assoc := ("jobId", Pbrt_yojson.make_string v.job_id) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_get_job_status_request (v:get_job_status_request) = 
  let assoc = ref [] in
  if get_job_status_request_has_job_id v then (
    assoc := ("jobId", Pbrt_yojson.make_string v.job_id) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_job_status (v:job_status) = 
  match v with
  | Queued -> `String "QUEUED"
  | Running -> `String "RUNNING"
  | Completed -> `String "COMPLETED"
  | Cancelled -> `String "CANCELLED"
  | Failed -> `String "FAILED"

let rec encode_json_get_job_status_response (v:get_job_status_response) = 
  let assoc = ref [] in
  if get_job_status_response_has_job_id v then (
    assoc := ("jobId", Pbrt_yojson.make_string v.job_id) :: !assoc;
  );
  if get_job_status_response_has_status v then (
    assoc := ("status", encode_json_job_status v.status) :: !assoc;
  );
  if get_job_status_response_has_progress_percent v then (
    assoc := ("progressPercent", Pbrt_yojson.make_int (Int32.to_int v.progress_percent)) :: !assoc;
  );
  if get_job_status_response_has_result_file v then (
    assoc := ("resultFile", Pbrt_yojson.make_string v.result_file) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_cancel_job_request (v:cancel_job_request) = 
  let assoc = ref [] in
  if cancel_job_request_has_job_id v then (
    assoc := ("jobId", Pbrt_yojson.make_string v.job_id) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_cancel_job_response (v:cancel_job_response) = 
Pbrt_yojson.make_unit v

[@@@ocaml.warning "-23-27-30-39"]

(** {2 JSON Decoding} *)

let rec decode_json_new_job_request d =
  let v = default_new_job_request () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("provers", `List l) -> begin
      new_job_request_set_provers v @@ List.map (function
        | json_value -> Pbrt_yojson.string json_value "new_job_request" "provers"
      ) l;
    end
    | ("paths", `List l) -> begin
      new_job_request_set_paths v @@ List.map (function
        | json_value -> Pbrt_yojson.string json_value "new_job_request" "paths"
      ) l;
    end
    | ("timeoutS", json_value) -> 
      new_job_request_set_timeout_s v (Pbrt_yojson.int32 json_value "new_job_request" "timeout_s")
    | ("memoryMb", json_value) -> 
      new_job_request_set_memory_mb v (Pbrt_yojson.int32 json_value "new_job_request" "memory_mb")
    | ("outputName", json_value) -> 
      new_job_request_set_output_name v (Pbrt_yojson.string json_value "new_job_request" "output_name")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    provers = v.provers;
    paths = v.paths;
    timeout_s = v.timeout_s;
    memory_mb = v.memory_mb;
    output_name = v.output_name;
  } : new_job_request)

let rec decode_json_new_job_response d =
  let v = default_new_job_response () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("jobId", json_value) -> 
      new_job_response_set_job_id v (Pbrt_yojson.string json_value "new_job_response" "job_id")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    job_id = v.job_id;
  } : new_job_response)

let rec decode_json_get_job_status_request d =
  let v = default_get_job_status_request () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("jobId", json_value) -> 
      get_job_status_request_set_job_id v (Pbrt_yojson.string json_value "get_job_status_request" "job_id")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    job_id = v.job_id;
  } : get_job_status_request)

let rec decode_json_job_status json =
  match json with
  | `String "QUEUED" -> (Queued : job_status)
  | `String "RUNNING" -> (Running : job_status)
  | `String "COMPLETED" -> (Completed : job_status)
  | `String "CANCELLED" -> (Cancelled : job_status)
  | `String "FAILED" -> (Failed : job_status)
  | _ -> Pbrt_yojson.E.malformed_variant "job_status"

let rec decode_json_get_job_status_response d =
  let v = default_get_job_status_response () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("jobId", json_value) -> 
      get_job_status_response_set_job_id v (Pbrt_yojson.string json_value "get_job_status_response" "job_id")
    | ("status", json_value) -> 
      get_job_status_response_set_status v ((decode_json_job_status json_value))
    | ("progressPercent", json_value) -> 
      get_job_status_response_set_progress_percent v (Pbrt_yojson.int32 json_value "get_job_status_response" "progress_percent")
    | ("resultFile", json_value) -> 
      get_job_status_response_set_result_file v (Pbrt_yojson.string json_value "get_job_status_response" "result_file")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    job_id = v.job_id;
    status = v.status;
    progress_percent = v.progress_percent;
    result_file = v.result_file;
  } : get_job_status_response)

let rec decode_json_cancel_job_request d =
  let v = default_cancel_job_request () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("jobId", json_value) -> 
      cancel_job_request_set_job_id v (Pbrt_yojson.string json_value "cancel_job_request" "job_id")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    job_id = v.job_id;
  } : cancel_job_request)

let rec decode_json_cancel_job_response d =
Pbrt_yojson.unit d "cancel_job_response" "empty record"

module BenchpressApi = struct
  open Pbrt_services.Value_mode
  module Client = struct
    open Pbrt_services
    
    let newJob : (new_job_request, unary, new_job_response, unary) Client.rpc =
      (Client.mk_rpc 
        ~package:["benchpress_api"]
        ~service_name:"BenchpressApi" ~rpc_name:"NewJob"
        ~req_mode:Client.Unary
        ~res_mode:Client.Unary
        ~encode_json_req:encode_json_new_job_request
        ~encode_pb_req:encode_pb_new_job_request
        ~decode_json_res:decode_json_new_job_response
        ~decode_pb_res:decode_pb_new_job_response
        () : (new_job_request, unary, new_job_response, unary) Client.rpc)
    open Pbrt_services
    
    let getJobStatus : (get_job_status_request, unary, get_job_status_response, unary) Client.rpc =
      (Client.mk_rpc 
        ~package:["benchpress_api"]
        ~service_name:"BenchpressApi" ~rpc_name:"GetJobStatus"
        ~req_mode:Client.Unary
        ~res_mode:Client.Unary
        ~encode_json_req:encode_json_get_job_status_request
        ~encode_pb_req:encode_pb_get_job_status_request
        ~decode_json_res:decode_json_get_job_status_response
        ~decode_pb_res:decode_pb_get_job_status_response
        () : (get_job_status_request, unary, get_job_status_response, unary) Client.rpc)
    open Pbrt_services
    
    let cancelJob : (cancel_job_request, unary, cancel_job_response, unary) Client.rpc =
      (Client.mk_rpc 
        ~package:["benchpress_api"]
        ~service_name:"BenchpressApi" ~rpc_name:"CancelJob"
        ~req_mode:Client.Unary
        ~res_mode:Client.Unary
        ~encode_json_req:encode_json_cancel_job_request
        ~encode_pb_req:encode_pb_cancel_job_request
        ~decode_json_res:decode_json_cancel_job_response
        ~decode_pb_res:decode_pb_cancel_job_response
        () : (cancel_job_request, unary, cancel_job_response, unary) Client.rpc)
  end
  
  module Server = struct
    open Pbrt_services
    
    let newJob : (new_job_request,unary,new_job_response,unary) Server.rpc = 
      (Server.mk_rpc ~name:"NewJob"
        ~req_mode:Server.Unary
        ~res_mode:Server.Unary
        ~encode_json_res:encode_json_new_job_response
        ~encode_pb_res:encode_pb_new_job_response
        ~decode_json_req:decode_json_new_job_request
        ~decode_pb_req:decode_pb_new_job_request
        () : _ Server.rpc)
    
    let getJobStatus : (get_job_status_request,unary,get_job_status_response,unary) Server.rpc = 
      (Server.mk_rpc ~name:"GetJobStatus"
        ~req_mode:Server.Unary
        ~res_mode:Server.Unary
        ~encode_json_res:encode_json_get_job_status_response
        ~encode_pb_res:encode_pb_get_job_status_response
        ~decode_json_req:decode_json_get_job_status_request
        ~decode_pb_req:decode_pb_get_job_status_request
        () : _ Server.rpc)
    
    let cancelJob : (cancel_job_request,unary,cancel_job_response,unary) Server.rpc = 
      (Server.mk_rpc ~name:"CancelJob"
        ~req_mode:Server.Unary
        ~res_mode:Server.Unary
        ~encode_json_res:encode_json_cancel_job_response
        ~encode_pb_res:encode_pb_cancel_job_response
        ~decode_json_req:decode_json_cancel_job_request
        ~decode_pb_req:decode_pb_cancel_job_request
        () : _ Server.rpc)
    
    let make
      ~newJob:__handler__newJob
      ~getJobStatus:__handler__getJobStatus
      ~cancelJob:__handler__cancelJob
      () : _ Server.t =
      { Server.
        service_name="BenchpressApi";
        package=["benchpress_api"];
        handlers=[
           (__handler__newJob newJob);
           (__handler__getJobStatus getJobStatus);
           (__handler__cancelJob cancelJob);
        ];
      }
  end
  
end

[@@@ocaml.warning "-23-27-30-39-44"]

type tools_capability = unit

type server_capabilities = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable tools : unit;
}

type server_info = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable name : string;
  mutable version : string;
}

type initialize_result = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable protocol_version : string;
  mutable capabilities : server_capabilities option;
  mutable server_info : server_info option;
}

type tool_def = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 3 fields *)
  mutable name : string;
  mutable description : string;
  mutable input_schema_json : string;
}

type tools_list_result = {
  mutable tools : tool_def list;
}

type tool_call_result = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable content_json : string;
}

type job_summary = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 6 fields *)
  mutable file : string;
  mutable uuid : string;
  mutable timestamp : float;
  mutable n_results : int32;
  mutable n_bad : int32;
  mutable provers : string list;
  mutable size_bytes : int32;
}

type list_jobs_result = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable jobs : job_summary list;
  mutable offset : int32;
  mutable limit : int32;
}

type prover_stat = {
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

type job_status_result = {
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

type result_entry = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 3 fields *)
  mutable prover : string;
  mutable result : string;
  mutable time_s : float;
}

type result_row = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable problem : string;
  mutable results : result_entry list;
}

type job_results_result = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable file : string;
  mutable provers : string list;
  mutable rows : result_row list;
  mutable n_rows : int32;
}

type query_item = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 5 fields *)
  mutable prover : string;
  mutable problem : string;
  mutable result : string;
  mutable expected : string;
  mutable time_s : float;
}

type query_job_results_result = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 5 fields *)
  mutable file : string;
  mutable total : int32;
  mutable page : int32;
  mutable per_page : int32;
  mutable is_complete : bool;
  mutable items : query_item list;
}

let default_tools_capability : tools_capability = ()

let default_server_capabilities (): server_capabilities =
{
  _presence=Pbrt.Bitfield.empty;
  tools=();
}

let default_server_info (): server_info =
{
  _presence=Pbrt.Bitfield.empty;
  name="";
  version="";
}

let default_initialize_result (): initialize_result =
{
  _presence=Pbrt.Bitfield.empty;
  protocol_version="";
  capabilities=None;
  server_info=None;
}

let default_tool_def (): tool_def =
{
  _presence=Pbrt.Bitfield.empty;
  name="";
  description="";
  input_schema_json="";
}

let default_tools_list_result (): tools_list_result =
{
  tools=[];
}

let default_tool_call_result (): tool_call_result =
{
  _presence=Pbrt.Bitfield.empty;
  content_json="";
}

let default_job_summary (): job_summary =
{
  _presence=Pbrt.Bitfield.empty;
  file="";
  uuid="";
  timestamp=0.;
  n_results=0l;
  n_bad=0l;
  provers=[];
  size_bytes=0l;
}

let default_list_jobs_result (): list_jobs_result =
{
  _presence=Pbrt.Bitfield.empty;
  jobs=[];
  offset=0l;
  limit=0l;
}

let default_prover_stat (): prover_stat =
{
  _presence=Pbrt.Bitfield.empty;
  prover="";
  sat=0l;
  unsat=0l;
  errors=0l;
  timeout=0l;
  unknown=0l;
  memory=0l;
  total=0l;
  total_time=0.;
  total_time_solved=0.;
}

let default_job_status_result (): job_status_result =
{
  _presence=Pbrt.Bitfield.empty;
  file="";
  uuid="";
  timestamp=0.;
  total_wall_time=0.;
  n_results=0l;
  n_bad=0l;
  is_complete=false;
  provers=[];
}

let default_result_entry (): result_entry =
{
  _presence=Pbrt.Bitfield.empty;
  prover="";
  result="";
  time_s=0.;
}

let default_result_row (): result_row =
{
  _presence=Pbrt.Bitfield.empty;
  problem="";
  results=[];
}

let default_job_results_result (): job_results_result =
{
  _presence=Pbrt.Bitfield.empty;
  file="";
  provers=[];
  rows=[];
  n_rows=0l;
}

let default_query_item (): query_item =
{
  _presence=Pbrt.Bitfield.empty;
  prover="";
  problem="";
  result="";
  expected="";
  time_s=0.;
}

let default_query_job_results_result (): query_job_results_result =
{
  _presence=Pbrt.Bitfield.empty;
  file="";
  total=0l;
  page=0l;
  per_page=0l;
  is_complete=false;
  items=[];
}


(** {2 Make functions} *)

let[@inline] server_capabilities_has_tools (self:server_capabilities) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] server_capabilities_set_tools (self:server_capabilities) (x:unit) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.tools <- x

let copy_server_capabilities (self:server_capabilities) : server_capabilities =
  { self with tools = self.tools }

let make_server_capabilities 
  ?(tools:unit option)
  () : server_capabilities  =
  let _res = default_server_capabilities () in
  (match tools with
  | None -> ()
  | Some v -> server_capabilities_set_tools _res v);
  _res

let[@inline] server_info_has_name (self:server_info) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] server_info_has_version (self:server_info) : bool = (Pbrt.Bitfield.get self._presence 1)

let[@inline] server_info_set_name (self:server_info) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.name <- x
let[@inline] server_info_set_version (self:server_info) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.version <- x

let copy_server_info (self:server_info) : server_info =
  { self with name = self.name }

let make_server_info 
  ?(name:string option)
  ?(version:string option)
  () : server_info  =
  let _res = default_server_info () in
  (match name with
  | None -> ()
  | Some v -> server_info_set_name _res v);
  (match version with
  | None -> ()
  | Some v -> server_info_set_version _res v);
  _res

let[@inline] initialize_result_has_protocol_version (self:initialize_result) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] initialize_result_has_capabilities (self:initialize_result) : bool = self.capabilities != None
let[@inline] initialize_result_has_server_info (self:initialize_result) : bool = self.server_info != None

let[@inline] initialize_result_set_protocol_version (self:initialize_result) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.protocol_version <- x
let[@inline] initialize_result_set_capabilities (self:initialize_result) (x:server_capabilities) : unit =
  self.capabilities <- Some x
let[@inline] initialize_result_set_server_info (self:initialize_result) (x:server_info) : unit =
  self.server_info <- Some x

let copy_initialize_result (self:initialize_result) : initialize_result =
  { self with protocol_version = self.protocol_version }

let make_initialize_result 
  ?(protocol_version:string option)
  ?(capabilities:server_capabilities option)
  ?(server_info:server_info option)
  () : initialize_result  =
  let _res = default_initialize_result () in
  (match protocol_version with
  | None -> ()
  | Some v -> initialize_result_set_protocol_version _res v);
  (match capabilities with
  | None -> ()
  | Some v -> initialize_result_set_capabilities _res v);
  (match server_info with
  | None -> ()
  | Some v -> initialize_result_set_server_info _res v);
  _res

let[@inline] tool_def_has_name (self:tool_def) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] tool_def_has_description (self:tool_def) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] tool_def_has_input_schema_json (self:tool_def) : bool = (Pbrt.Bitfield.get self._presence 2)

let[@inline] tool_def_set_name (self:tool_def) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.name <- x
let[@inline] tool_def_set_description (self:tool_def) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.description <- x
let[@inline] tool_def_set_input_schema_json (self:tool_def) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.input_schema_json <- x

let copy_tool_def (self:tool_def) : tool_def =
  { self with name = self.name }

let make_tool_def 
  ?(name:string option)
  ?(description:string option)
  ?(input_schema_json:string option)
  () : tool_def  =
  let _res = default_tool_def () in
  (match name with
  | None -> ()
  | Some v -> tool_def_set_name _res v);
  (match description with
  | None -> ()
  | Some v -> tool_def_set_description _res v);
  (match input_schema_json with
  | None -> ()
  | Some v -> tool_def_set_input_schema_json _res v);
  _res


let[@inline] tools_list_result_set_tools (self:tools_list_result) (x:tool_def list) : unit =
  self.tools <- x

let copy_tools_list_result (self:tools_list_result) : tools_list_result =
  { self with tools = self.tools }

let make_tools_list_result 
  ?(tools=[])
  () : tools_list_result  =
  let _res = default_tools_list_result () in
  tools_list_result_set_tools _res tools;
  _res

let[@inline] tool_call_result_has_content_json (self:tool_call_result) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] tool_call_result_set_content_json (self:tool_call_result) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.content_json <- x

let copy_tool_call_result (self:tool_call_result) : tool_call_result =
  { self with content_json = self.content_json }

let make_tool_call_result 
  ?(content_json:string option)
  () : tool_call_result  =
  let _res = default_tool_call_result () in
  (match content_json with
  | None -> ()
  | Some v -> tool_call_result_set_content_json _res v);
  _res

let[@inline] job_summary_has_file (self:job_summary) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] job_summary_has_uuid (self:job_summary) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] job_summary_has_timestamp (self:job_summary) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] job_summary_has_n_results (self:job_summary) : bool = (Pbrt.Bitfield.get self._presence 3)
let[@inline] job_summary_has_n_bad (self:job_summary) : bool = (Pbrt.Bitfield.get self._presence 4)
let[@inline] job_summary_has_size_bytes (self:job_summary) : bool = (Pbrt.Bitfield.get self._presence 5)

let[@inline] job_summary_set_file (self:job_summary) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.file <- x
let[@inline] job_summary_set_uuid (self:job_summary) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.uuid <- x
let[@inline] job_summary_set_timestamp (self:job_summary) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.timestamp <- x
let[@inline] job_summary_set_n_results (self:job_summary) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.n_results <- x
let[@inline] job_summary_set_n_bad (self:job_summary) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 4); self.n_bad <- x
let[@inline] job_summary_set_provers (self:job_summary) (x:string list) : unit =
  self.provers <- x
let[@inline] job_summary_set_size_bytes (self:job_summary) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 5); self.size_bytes <- x

let copy_job_summary (self:job_summary) : job_summary =
  { self with file = self.file }

let make_job_summary 
  ?(file:string option)
  ?(uuid:string option)
  ?(timestamp:float option)
  ?(n_results:int32 option)
  ?(n_bad:int32 option)
  ?(provers=[])
  ?(size_bytes:int32 option)
  () : job_summary  =
  let _res = default_job_summary () in
  (match file with
  | None -> ()
  | Some v -> job_summary_set_file _res v);
  (match uuid with
  | None -> ()
  | Some v -> job_summary_set_uuid _res v);
  (match timestamp with
  | None -> ()
  | Some v -> job_summary_set_timestamp _res v);
  (match n_results with
  | None -> ()
  | Some v -> job_summary_set_n_results _res v);
  (match n_bad with
  | None -> ()
  | Some v -> job_summary_set_n_bad _res v);
  job_summary_set_provers _res provers;
  (match size_bytes with
  | None -> ()
  | Some v -> job_summary_set_size_bytes _res v);
  _res

let[@inline] list_jobs_result_has_offset (self:list_jobs_result) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] list_jobs_result_has_limit (self:list_jobs_result) : bool = (Pbrt.Bitfield.get self._presence 1)

let[@inline] list_jobs_result_set_jobs (self:list_jobs_result) (x:job_summary list) : unit =
  self.jobs <- x
let[@inline] list_jobs_result_set_offset (self:list_jobs_result) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.offset <- x
let[@inline] list_jobs_result_set_limit (self:list_jobs_result) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.limit <- x

let copy_list_jobs_result (self:list_jobs_result) : list_jobs_result =
  { self with jobs = self.jobs }

let make_list_jobs_result 
  ?(jobs=[])
  ?(offset:int32 option)
  ?(limit:int32 option)
  () : list_jobs_result  =
  let _res = default_list_jobs_result () in
  list_jobs_result_set_jobs _res jobs;
  (match offset with
  | None -> ()
  | Some v -> list_jobs_result_set_offset _res v);
  (match limit with
  | None -> ()
  | Some v -> list_jobs_result_set_limit _res v);
  _res

let[@inline] prover_stat_has_prover (self:prover_stat) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] prover_stat_has_sat (self:prover_stat) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] prover_stat_has_unsat (self:prover_stat) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] prover_stat_has_errors (self:prover_stat) : bool = (Pbrt.Bitfield.get self._presence 3)
let[@inline] prover_stat_has_timeout (self:prover_stat) : bool = (Pbrt.Bitfield.get self._presence 4)
let[@inline] prover_stat_has_unknown (self:prover_stat) : bool = (Pbrt.Bitfield.get self._presence 5)
let[@inline] prover_stat_has_memory (self:prover_stat) : bool = (Pbrt.Bitfield.get self._presence 6)
let[@inline] prover_stat_has_total (self:prover_stat) : bool = (Pbrt.Bitfield.get self._presence 7)
let[@inline] prover_stat_has_total_time (self:prover_stat) : bool = (Pbrt.Bitfield.get self._presence 8)
let[@inline] prover_stat_has_total_time_solved (self:prover_stat) : bool = (Pbrt.Bitfield.get self._presence 9)

let[@inline] prover_stat_set_prover (self:prover_stat) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.prover <- x
let[@inline] prover_stat_set_sat (self:prover_stat) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.sat <- x
let[@inline] prover_stat_set_unsat (self:prover_stat) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.unsat <- x
let[@inline] prover_stat_set_errors (self:prover_stat) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.errors <- x
let[@inline] prover_stat_set_timeout (self:prover_stat) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 4); self.timeout <- x
let[@inline] prover_stat_set_unknown (self:prover_stat) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 5); self.unknown <- x
let[@inline] prover_stat_set_memory (self:prover_stat) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 6); self.memory <- x
let[@inline] prover_stat_set_total (self:prover_stat) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 7); self.total <- x
let[@inline] prover_stat_set_total_time (self:prover_stat) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 8); self.total_time <- x
let[@inline] prover_stat_set_total_time_solved (self:prover_stat) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 9); self.total_time_solved <- x

let copy_prover_stat (self:prover_stat) : prover_stat =
  { self with prover = self.prover }

let make_prover_stat 
  ?(prover:string option)
  ?(sat:int32 option)
  ?(unsat:int32 option)
  ?(errors:int32 option)
  ?(timeout:int32 option)
  ?(unknown:int32 option)
  ?(memory:int32 option)
  ?(total:int32 option)
  ?(total_time:float option)
  ?(total_time_solved:float option)
  () : prover_stat  =
  let _res = default_prover_stat () in
  (match prover with
  | None -> ()
  | Some v -> prover_stat_set_prover _res v);
  (match sat with
  | None -> ()
  | Some v -> prover_stat_set_sat _res v);
  (match unsat with
  | None -> ()
  | Some v -> prover_stat_set_unsat _res v);
  (match errors with
  | None -> ()
  | Some v -> prover_stat_set_errors _res v);
  (match timeout with
  | None -> ()
  | Some v -> prover_stat_set_timeout _res v);
  (match unknown with
  | None -> ()
  | Some v -> prover_stat_set_unknown _res v);
  (match memory with
  | None -> ()
  | Some v -> prover_stat_set_memory _res v);
  (match total with
  | None -> ()
  | Some v -> prover_stat_set_total _res v);
  (match total_time with
  | None -> ()
  | Some v -> prover_stat_set_total_time _res v);
  (match total_time_solved with
  | None -> ()
  | Some v -> prover_stat_set_total_time_solved _res v);
  _res

let[@inline] job_status_result_has_file (self:job_status_result) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] job_status_result_has_uuid (self:job_status_result) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] job_status_result_has_timestamp (self:job_status_result) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] job_status_result_has_total_wall_time (self:job_status_result) : bool = (Pbrt.Bitfield.get self._presence 3)
let[@inline] job_status_result_has_n_results (self:job_status_result) : bool = (Pbrt.Bitfield.get self._presence 4)
let[@inline] job_status_result_has_n_bad (self:job_status_result) : bool = (Pbrt.Bitfield.get self._presence 5)
let[@inline] job_status_result_has_is_complete (self:job_status_result) : bool = (Pbrt.Bitfield.get self._presence 6)

let[@inline] job_status_result_set_file (self:job_status_result) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.file <- x
let[@inline] job_status_result_set_uuid (self:job_status_result) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.uuid <- x
let[@inline] job_status_result_set_timestamp (self:job_status_result) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.timestamp <- x
let[@inline] job_status_result_set_total_wall_time (self:job_status_result) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.total_wall_time <- x
let[@inline] job_status_result_set_n_results (self:job_status_result) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 4); self.n_results <- x
let[@inline] job_status_result_set_n_bad (self:job_status_result) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 5); self.n_bad <- x
let[@inline] job_status_result_set_is_complete (self:job_status_result) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 6); self.is_complete <- x
let[@inline] job_status_result_set_provers (self:job_status_result) (x:prover_stat list) : unit =
  self.provers <- x

let copy_job_status_result (self:job_status_result) : job_status_result =
  { self with file = self.file }

let make_job_status_result 
  ?(file:string option)
  ?(uuid:string option)
  ?(timestamp:float option)
  ?(total_wall_time:float option)
  ?(n_results:int32 option)
  ?(n_bad:int32 option)
  ?(is_complete:bool option)
  ?(provers=[])
  () : job_status_result  =
  let _res = default_job_status_result () in
  (match file with
  | None -> ()
  | Some v -> job_status_result_set_file _res v);
  (match uuid with
  | None -> ()
  | Some v -> job_status_result_set_uuid _res v);
  (match timestamp with
  | None -> ()
  | Some v -> job_status_result_set_timestamp _res v);
  (match total_wall_time with
  | None -> ()
  | Some v -> job_status_result_set_total_wall_time _res v);
  (match n_results with
  | None -> ()
  | Some v -> job_status_result_set_n_results _res v);
  (match n_bad with
  | None -> ()
  | Some v -> job_status_result_set_n_bad _res v);
  (match is_complete with
  | None -> ()
  | Some v -> job_status_result_set_is_complete _res v);
  job_status_result_set_provers _res provers;
  _res

let[@inline] result_entry_has_prover (self:result_entry) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] result_entry_has_result (self:result_entry) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] result_entry_has_time_s (self:result_entry) : bool = (Pbrt.Bitfield.get self._presence 2)

let[@inline] result_entry_set_prover (self:result_entry) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.prover <- x
let[@inline] result_entry_set_result (self:result_entry) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.result <- x
let[@inline] result_entry_set_time_s (self:result_entry) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.time_s <- x

let copy_result_entry (self:result_entry) : result_entry =
  { self with prover = self.prover }

let make_result_entry 
  ?(prover:string option)
  ?(result:string option)
  ?(time_s:float option)
  () : result_entry  =
  let _res = default_result_entry () in
  (match prover with
  | None -> ()
  | Some v -> result_entry_set_prover _res v);
  (match result with
  | None -> ()
  | Some v -> result_entry_set_result _res v);
  (match time_s with
  | None -> ()
  | Some v -> result_entry_set_time_s _res v);
  _res

let[@inline] result_row_has_problem (self:result_row) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] result_row_set_problem (self:result_row) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.problem <- x
let[@inline] result_row_set_results (self:result_row) (x:result_entry list) : unit =
  self.results <- x

let copy_result_row (self:result_row) : result_row =
  { self with problem = self.problem }

let make_result_row 
  ?(problem:string option)
  ?(results=[])
  () : result_row  =
  let _res = default_result_row () in
  (match problem with
  | None -> ()
  | Some v -> result_row_set_problem _res v);
  result_row_set_results _res results;
  _res

let[@inline] job_results_result_has_file (self:job_results_result) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] job_results_result_has_n_rows (self:job_results_result) : bool = (Pbrt.Bitfield.get self._presence 1)

let[@inline] job_results_result_set_file (self:job_results_result) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.file <- x
let[@inline] job_results_result_set_provers (self:job_results_result) (x:string list) : unit =
  self.provers <- x
let[@inline] job_results_result_set_rows (self:job_results_result) (x:result_row list) : unit =
  self.rows <- x
let[@inline] job_results_result_set_n_rows (self:job_results_result) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.n_rows <- x

let copy_job_results_result (self:job_results_result) : job_results_result =
  { self with file = self.file }

let make_job_results_result 
  ?(file:string option)
  ?(provers=[])
  ?(rows=[])
  ?(n_rows:int32 option)
  () : job_results_result  =
  let _res = default_job_results_result () in
  (match file with
  | None -> ()
  | Some v -> job_results_result_set_file _res v);
  job_results_result_set_provers _res provers;
  job_results_result_set_rows _res rows;
  (match n_rows with
  | None -> ()
  | Some v -> job_results_result_set_n_rows _res v);
  _res

let[@inline] query_item_has_prover (self:query_item) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] query_item_has_problem (self:query_item) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] query_item_has_result (self:query_item) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] query_item_has_expected (self:query_item) : bool = (Pbrt.Bitfield.get self._presence 3)
let[@inline] query_item_has_time_s (self:query_item) : bool = (Pbrt.Bitfield.get self._presence 4)

let[@inline] query_item_set_prover (self:query_item) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.prover <- x
let[@inline] query_item_set_problem (self:query_item) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.problem <- x
let[@inline] query_item_set_result (self:query_item) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.result <- x
let[@inline] query_item_set_expected (self:query_item) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.expected <- x
let[@inline] query_item_set_time_s (self:query_item) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 4); self.time_s <- x

let copy_query_item (self:query_item) : query_item =
  { self with prover = self.prover }

let make_query_item 
  ?(prover:string option)
  ?(problem:string option)
  ?(result:string option)
  ?(expected:string option)
  ?(time_s:float option)
  () : query_item  =
  let _res = default_query_item () in
  (match prover with
  | None -> ()
  | Some v -> query_item_set_prover _res v);
  (match problem with
  | None -> ()
  | Some v -> query_item_set_problem _res v);
  (match result with
  | None -> ()
  | Some v -> query_item_set_result _res v);
  (match expected with
  | None -> ()
  | Some v -> query_item_set_expected _res v);
  (match time_s with
  | None -> ()
  | Some v -> query_item_set_time_s _res v);
  _res

let[@inline] query_job_results_result_has_file (self:query_job_results_result) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] query_job_results_result_has_total (self:query_job_results_result) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] query_job_results_result_has_page (self:query_job_results_result) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] query_job_results_result_has_per_page (self:query_job_results_result) : bool = (Pbrt.Bitfield.get self._presence 3)
let[@inline] query_job_results_result_has_is_complete (self:query_job_results_result) : bool = (Pbrt.Bitfield.get self._presence 4)

let[@inline] query_job_results_result_set_file (self:query_job_results_result) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.file <- x
let[@inline] query_job_results_result_set_total (self:query_job_results_result) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.total <- x
let[@inline] query_job_results_result_set_page (self:query_job_results_result) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.page <- x
let[@inline] query_job_results_result_set_per_page (self:query_job_results_result) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.per_page <- x
let[@inline] query_job_results_result_set_is_complete (self:query_job_results_result) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 4); self.is_complete <- x
let[@inline] query_job_results_result_set_items (self:query_job_results_result) (x:query_item list) : unit =
  self.items <- x

let copy_query_job_results_result (self:query_job_results_result) : query_job_results_result =
  { self with file = self.file }

let make_query_job_results_result 
  ?(file:string option)
  ?(total:int32 option)
  ?(page:int32 option)
  ?(per_page:int32 option)
  ?(is_complete:bool option)
  ?(items=[])
  () : query_job_results_result  =
  let _res = default_query_job_results_result () in
  (match file with
  | None -> ()
  | Some v -> query_job_results_result_set_file _res v);
  (match total with
  | None -> ()
  | Some v -> query_job_results_result_set_total _res v);
  (match page with
  | None -> ()
  | Some v -> query_job_results_result_set_page _res v);
  (match per_page with
  | None -> ()
  | Some v -> query_job_results_result_set_per_page _res v);
  (match is_complete with
  | None -> ()
  | Some v -> query_job_results_result_set_is_complete _res v);
  query_job_results_result_set_items _res items;
  _res

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf YoJson Encoding} *)

let rec encode_json_tools_capability (v:tools_capability) = 
Pbrt_yojson.make_unit v

let rec encode_json_server_capabilities (v:server_capabilities) = 
  let assoc = ref [] in
  if server_capabilities_has_tools v then (
  );
  `Assoc !assoc

let rec encode_json_server_info (v:server_info) = 
  let assoc = ref [] in
  if server_info_has_name v then (
    assoc := ("name", Pbrt_yojson.make_string v.name) :: !assoc;
  );
  if server_info_has_version v then (
    assoc := ("version", Pbrt_yojson.make_string v.version) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_initialize_result (v:initialize_result) = 
  let assoc = ref [] in
  if initialize_result_has_protocol_version v then (
    assoc := ("protocolVersion", Pbrt_yojson.make_string v.protocol_version) :: !assoc;
  );
  assoc := (match v.capabilities with
    | None -> !assoc
    | Some v -> ("capabilities", encode_json_server_capabilities v) :: !assoc);
  assoc := (match v.server_info with
    | None -> !assoc
    | Some v -> ("serverInfo", encode_json_server_info v) :: !assoc);
  `Assoc !assoc

let rec encode_json_tool_def (v:tool_def) = 
  let assoc = ref [] in
  if tool_def_has_name v then (
    assoc := ("name", Pbrt_yojson.make_string v.name) :: !assoc;
  );
  if tool_def_has_description v then (
    assoc := ("description", Pbrt_yojson.make_string v.description) :: !assoc;
  );
  if tool_def_has_input_schema_json v then (
    assoc := ("inputSchemaJson", Pbrt_yojson.make_string v.input_schema_json) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_tools_list_result (v:tools_list_result) = 
  let assoc = ref [] in
  assoc := (
    let l = v.tools |> List.map encode_json_tool_def in
    ("tools", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_tool_call_result (v:tool_call_result) = 
  let assoc = ref [] in
  if tool_call_result_has_content_json v then (
    assoc := ("contentJson", Pbrt_yojson.make_string v.content_json) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_job_summary (v:job_summary) = 
  let assoc = ref [] in
  if job_summary_has_file v then (
    assoc := ("file", Pbrt_yojson.make_string v.file) :: !assoc;
  );
  if job_summary_has_uuid v then (
    assoc := ("uuid", Pbrt_yojson.make_string v.uuid) :: !assoc;
  );
  if job_summary_has_timestamp v then (
    assoc := ("timestamp", Pbrt_yojson.make_string (string_of_float v.timestamp)) :: !assoc;
  );
  if job_summary_has_n_results v then (
    assoc := ("nResults", Pbrt_yojson.make_int (Int32.to_int v.n_results)) :: !assoc;
  );
  if job_summary_has_n_bad v then (
    assoc := ("nBad", Pbrt_yojson.make_int (Int32.to_int v.n_bad)) :: !assoc;
  );
  assoc := (
    let l = v.provers |> List.map Pbrt_yojson.make_string in
    ("provers", `List l) :: !assoc 
  );
  if job_summary_has_size_bytes v then (
    assoc := ("sizeBytes", Pbrt_yojson.make_int (Int32.to_int v.size_bytes)) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_list_jobs_result (v:list_jobs_result) = 
  let assoc = ref [] in
  assoc := (
    let l = v.jobs |> List.map encode_json_job_summary in
    ("jobs", `List l) :: !assoc 
  );
  if list_jobs_result_has_offset v then (
    assoc := ("offset", Pbrt_yojson.make_int (Int32.to_int v.offset)) :: !assoc;
  );
  if list_jobs_result_has_limit v then (
    assoc := ("limit", Pbrt_yojson.make_int (Int32.to_int v.limit)) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_prover_stat (v:prover_stat) = 
  let assoc = ref [] in
  if prover_stat_has_prover v then (
    assoc := ("prover", Pbrt_yojson.make_string v.prover) :: !assoc;
  );
  if prover_stat_has_sat v then (
    assoc := ("sat", Pbrt_yojson.make_int (Int32.to_int v.sat)) :: !assoc;
  );
  if prover_stat_has_unsat v then (
    assoc := ("unsat", Pbrt_yojson.make_int (Int32.to_int v.unsat)) :: !assoc;
  );
  if prover_stat_has_errors v then (
    assoc := ("errors", Pbrt_yojson.make_int (Int32.to_int v.errors)) :: !assoc;
  );
  if prover_stat_has_timeout v then (
    assoc := ("timeout", Pbrt_yojson.make_int (Int32.to_int v.timeout)) :: !assoc;
  );
  if prover_stat_has_unknown v then (
    assoc := ("unknown", Pbrt_yojson.make_int (Int32.to_int v.unknown)) :: !assoc;
  );
  if prover_stat_has_memory v then (
    assoc := ("memory", Pbrt_yojson.make_int (Int32.to_int v.memory)) :: !assoc;
  );
  if prover_stat_has_total v then (
    assoc := ("total", Pbrt_yojson.make_int (Int32.to_int v.total)) :: !assoc;
  );
  if prover_stat_has_total_time v then (
    assoc := ("totalTime", Pbrt_yojson.make_string (string_of_float v.total_time)) :: !assoc;
  );
  if prover_stat_has_total_time_solved v then (
    assoc := ("totalTimeSolved", Pbrt_yojson.make_string (string_of_float v.total_time_solved)) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_job_status_result (v:job_status_result) = 
  let assoc = ref [] in
  if job_status_result_has_file v then (
    assoc := ("file", Pbrt_yojson.make_string v.file) :: !assoc;
  );
  if job_status_result_has_uuid v then (
    assoc := ("uuid", Pbrt_yojson.make_string v.uuid) :: !assoc;
  );
  if job_status_result_has_timestamp v then (
    assoc := ("timestamp", Pbrt_yojson.make_string (string_of_float v.timestamp)) :: !assoc;
  );
  if job_status_result_has_total_wall_time v then (
    assoc := ("totalWallTime", Pbrt_yojson.make_string (string_of_float v.total_wall_time)) :: !assoc;
  );
  if job_status_result_has_n_results v then (
    assoc := ("nResults", Pbrt_yojson.make_int (Int32.to_int v.n_results)) :: !assoc;
  );
  if job_status_result_has_n_bad v then (
    assoc := ("nBad", Pbrt_yojson.make_int (Int32.to_int v.n_bad)) :: !assoc;
  );
  if job_status_result_has_is_complete v then (
    assoc := ("isComplete", Pbrt_yojson.make_bool v.is_complete) :: !assoc;
  );
  assoc := (
    let l = v.provers |> List.map encode_json_prover_stat in
    ("provers", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_result_entry (v:result_entry) = 
  let assoc = ref [] in
  if result_entry_has_prover v then (
    assoc := ("prover", Pbrt_yojson.make_string v.prover) :: !assoc;
  );
  if result_entry_has_result v then (
    assoc := ("result", Pbrt_yojson.make_string v.result) :: !assoc;
  );
  if result_entry_has_time_s v then (
    assoc := ("timeS", Pbrt_yojson.make_string (string_of_float v.time_s)) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_result_row (v:result_row) = 
  let assoc = ref [] in
  if result_row_has_problem v then (
    assoc := ("problem", Pbrt_yojson.make_string v.problem) :: !assoc;
  );
  assoc := (
    let l = v.results |> List.map encode_json_result_entry in
    ("results", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_job_results_result (v:job_results_result) = 
  let assoc = ref [] in
  if job_results_result_has_file v then (
    assoc := ("file", Pbrt_yojson.make_string v.file) :: !assoc;
  );
  assoc := (
    let l = v.provers |> List.map Pbrt_yojson.make_string in
    ("provers", `List l) :: !assoc 
  );
  assoc := (
    let l = v.rows |> List.map encode_json_result_row in
    ("rows", `List l) :: !assoc 
  );
  if job_results_result_has_n_rows v then (
    assoc := ("nRows", Pbrt_yojson.make_int (Int32.to_int v.n_rows)) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_query_item (v:query_item) = 
  let assoc = ref [] in
  if query_item_has_prover v then (
    assoc := ("prover", Pbrt_yojson.make_string v.prover) :: !assoc;
  );
  if query_item_has_problem v then (
    assoc := ("problem", Pbrt_yojson.make_string v.problem) :: !assoc;
  );
  if query_item_has_result v then (
    assoc := ("result", Pbrt_yojson.make_string v.result) :: !assoc;
  );
  if query_item_has_expected v then (
    assoc := ("expected", Pbrt_yojson.make_string v.expected) :: !assoc;
  );
  if query_item_has_time_s v then (
    assoc := ("timeS", Pbrt_yojson.make_string (string_of_float v.time_s)) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_query_job_results_result (v:query_job_results_result) = 
  let assoc = ref [] in
  if query_job_results_result_has_file v then (
    assoc := ("file", Pbrt_yojson.make_string v.file) :: !assoc;
  );
  if query_job_results_result_has_total v then (
    assoc := ("total", Pbrt_yojson.make_int (Int32.to_int v.total)) :: !assoc;
  );
  if query_job_results_result_has_page v then (
    assoc := ("page", Pbrt_yojson.make_int (Int32.to_int v.page)) :: !assoc;
  );
  if query_job_results_result_has_per_page v then (
    assoc := ("perPage", Pbrt_yojson.make_int (Int32.to_int v.per_page)) :: !assoc;
  );
  if query_job_results_result_has_is_complete v then (
    assoc := ("isComplete", Pbrt_yojson.make_bool v.is_complete) :: !assoc;
  );
  assoc := (
    let l = v.items |> List.map encode_json_query_item in
    ("items", `List l) :: !assoc 
  );
  `Assoc !assoc

[@@@ocaml.warning "-23-27-30-39"]

(** {2 JSON Decoding} *)

let rec decode_json_tools_capability d =
Pbrt_yojson.unit d "tools_capability" "empty record"

let rec decode_json_server_capabilities d =
  let v = default_server_capabilities () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("tools", json_value) -> 
      server_capabilities_set_tools v (Pbrt_yojson.unit json_value "server_capabilities" "tools")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    tools = v.tools;
  } : server_capabilities)

let rec decode_json_server_info d =
  let v = default_server_info () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("name", json_value) -> 
      server_info_set_name v (Pbrt_yojson.string json_value "server_info" "name")
    | ("version", json_value) -> 
      server_info_set_version v (Pbrt_yojson.string json_value "server_info" "version")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    name = v.name;
    version = v.version;
  } : server_info)

let rec decode_json_initialize_result d =
  let v = default_initialize_result () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("protocolVersion", json_value) -> 
      initialize_result_set_protocol_version v (Pbrt_yojson.string json_value "initialize_result" "protocol_version")
    | ("capabilities", json_value) -> 
      initialize_result_set_capabilities v (decode_json_server_capabilities json_value)
    | ("serverInfo", json_value) -> 
      initialize_result_set_server_info v (decode_json_server_info json_value)
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    protocol_version = v.protocol_version;
    capabilities = v.capabilities;
    server_info = v.server_info;
  } : initialize_result)

let rec decode_json_tool_def d =
  let v = default_tool_def () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("name", json_value) -> 
      tool_def_set_name v (Pbrt_yojson.string json_value "tool_def" "name")
    | ("description", json_value) -> 
      tool_def_set_description v (Pbrt_yojson.string json_value "tool_def" "description")
    | ("inputSchemaJson", json_value) -> 
      tool_def_set_input_schema_json v (Pbrt_yojson.string json_value "tool_def" "input_schema_json")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    name = v.name;
    description = v.description;
    input_schema_json = v.input_schema_json;
  } : tool_def)

let rec decode_json_tools_list_result d =
  let v = default_tools_list_result () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("tools", `List l) -> begin
      tools_list_result_set_tools v @@ List.map (function
        | json_value -> (decode_json_tool_def json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    tools = v.tools;
  } : tools_list_result)

let rec decode_json_tool_call_result d =
  let v = default_tool_call_result () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("contentJson", json_value) -> 
      tool_call_result_set_content_json v (Pbrt_yojson.string json_value "tool_call_result" "content_json")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    content_json = v.content_json;
  } : tool_call_result)

let rec decode_json_job_summary d =
  let v = default_job_summary () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("file", json_value) -> 
      job_summary_set_file v (Pbrt_yojson.string json_value "job_summary" "file")
    | ("uuid", json_value) -> 
      job_summary_set_uuid v (Pbrt_yojson.string json_value "job_summary" "uuid")
    | ("timestamp", json_value) -> 
      job_summary_set_timestamp v (Pbrt_yojson.float json_value "job_summary" "timestamp")
    | ("nResults", json_value) -> 
      job_summary_set_n_results v (Pbrt_yojson.int32 json_value "job_summary" "n_results")
    | ("nBad", json_value) -> 
      job_summary_set_n_bad v (Pbrt_yojson.int32 json_value "job_summary" "n_bad")
    | ("provers", `List l) -> begin
      job_summary_set_provers v @@ List.map (function
        | json_value -> Pbrt_yojson.string json_value "job_summary" "provers"
      ) l;
    end
    | ("sizeBytes", json_value) -> 
      job_summary_set_size_bytes v (Pbrt_yojson.int32 json_value "job_summary" "size_bytes")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    file = v.file;
    uuid = v.uuid;
    timestamp = v.timestamp;
    n_results = v.n_results;
    n_bad = v.n_bad;
    provers = v.provers;
    size_bytes = v.size_bytes;
  } : job_summary)

let rec decode_json_list_jobs_result d =
  let v = default_list_jobs_result () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("jobs", `List l) -> begin
      list_jobs_result_set_jobs v @@ List.map (function
        | json_value -> (decode_json_job_summary json_value)
      ) l;
    end
    | ("offset", json_value) -> 
      list_jobs_result_set_offset v (Pbrt_yojson.int32 json_value "list_jobs_result" "offset")
    | ("limit", json_value) -> 
      list_jobs_result_set_limit v (Pbrt_yojson.int32 json_value "list_jobs_result" "limit")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    jobs = v.jobs;
    offset = v.offset;
    limit = v.limit;
  } : list_jobs_result)

let rec decode_json_prover_stat d =
  let v = default_prover_stat () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("prover", json_value) -> 
      prover_stat_set_prover v (Pbrt_yojson.string json_value "prover_stat" "prover")
    | ("sat", json_value) -> 
      prover_stat_set_sat v (Pbrt_yojson.int32 json_value "prover_stat" "sat")
    | ("unsat", json_value) -> 
      prover_stat_set_unsat v (Pbrt_yojson.int32 json_value "prover_stat" "unsat")
    | ("errors", json_value) -> 
      prover_stat_set_errors v (Pbrt_yojson.int32 json_value "prover_stat" "errors")
    | ("timeout", json_value) -> 
      prover_stat_set_timeout v (Pbrt_yojson.int32 json_value "prover_stat" "timeout")
    | ("unknown", json_value) -> 
      prover_stat_set_unknown v (Pbrt_yojson.int32 json_value "prover_stat" "unknown")
    | ("memory", json_value) -> 
      prover_stat_set_memory v (Pbrt_yojson.int32 json_value "prover_stat" "memory")
    | ("total", json_value) -> 
      prover_stat_set_total v (Pbrt_yojson.int32 json_value "prover_stat" "total")
    | ("totalTime", json_value) -> 
      prover_stat_set_total_time v (Pbrt_yojson.float json_value "prover_stat" "total_time")
    | ("totalTimeSolved", json_value) -> 
      prover_stat_set_total_time_solved v (Pbrt_yojson.float json_value "prover_stat" "total_time_solved")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    prover = v.prover;
    sat = v.sat;
    unsat = v.unsat;
    errors = v.errors;
    timeout = v.timeout;
    unknown = v.unknown;
    memory = v.memory;
    total = v.total;
    total_time = v.total_time;
    total_time_solved = v.total_time_solved;
  } : prover_stat)

let rec decode_json_job_status_result d =
  let v = default_job_status_result () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("file", json_value) -> 
      job_status_result_set_file v (Pbrt_yojson.string json_value "job_status_result" "file")
    | ("uuid", json_value) -> 
      job_status_result_set_uuid v (Pbrt_yojson.string json_value "job_status_result" "uuid")
    | ("timestamp", json_value) -> 
      job_status_result_set_timestamp v (Pbrt_yojson.float json_value "job_status_result" "timestamp")
    | ("totalWallTime", json_value) -> 
      job_status_result_set_total_wall_time v (Pbrt_yojson.float json_value "job_status_result" "total_wall_time")
    | ("nResults", json_value) -> 
      job_status_result_set_n_results v (Pbrt_yojson.int32 json_value "job_status_result" "n_results")
    | ("nBad", json_value) -> 
      job_status_result_set_n_bad v (Pbrt_yojson.int32 json_value "job_status_result" "n_bad")
    | ("isComplete", json_value) -> 
      job_status_result_set_is_complete v (Pbrt_yojson.bool json_value "job_status_result" "is_complete")
    | ("provers", `List l) -> begin
      job_status_result_set_provers v @@ List.map (function
        | json_value -> (decode_json_prover_stat json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    file = v.file;
    uuid = v.uuid;
    timestamp = v.timestamp;
    total_wall_time = v.total_wall_time;
    n_results = v.n_results;
    n_bad = v.n_bad;
    is_complete = v.is_complete;
    provers = v.provers;
  } : job_status_result)

let rec decode_json_result_entry d =
  let v = default_result_entry () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("prover", json_value) -> 
      result_entry_set_prover v (Pbrt_yojson.string json_value "result_entry" "prover")
    | ("result", json_value) -> 
      result_entry_set_result v (Pbrt_yojson.string json_value "result_entry" "result")
    | ("timeS", json_value) -> 
      result_entry_set_time_s v (Pbrt_yojson.float json_value "result_entry" "time_s")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    prover = v.prover;
    result = v.result;
    time_s = v.time_s;
  } : result_entry)

let rec decode_json_result_row d =
  let v = default_result_row () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("problem", json_value) -> 
      result_row_set_problem v (Pbrt_yojson.string json_value "result_row" "problem")
    | ("results", `List l) -> begin
      result_row_set_results v @@ List.map (function
        | json_value -> (decode_json_result_entry json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    problem = v.problem;
    results = v.results;
  } : result_row)

let rec decode_json_job_results_result d =
  let v = default_job_results_result () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("file", json_value) -> 
      job_results_result_set_file v (Pbrt_yojson.string json_value "job_results_result" "file")
    | ("provers", `List l) -> begin
      job_results_result_set_provers v @@ List.map (function
        | json_value -> Pbrt_yojson.string json_value "job_results_result" "provers"
      ) l;
    end
    | ("rows", `List l) -> begin
      job_results_result_set_rows v @@ List.map (function
        | json_value -> (decode_json_result_row json_value)
      ) l;
    end
    | ("nRows", json_value) -> 
      job_results_result_set_n_rows v (Pbrt_yojson.int32 json_value "job_results_result" "n_rows")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    file = v.file;
    provers = v.provers;
    rows = v.rows;
    n_rows = v.n_rows;
  } : job_results_result)

let rec decode_json_query_item d =
  let v = default_query_item () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("prover", json_value) -> 
      query_item_set_prover v (Pbrt_yojson.string json_value "query_item" "prover")
    | ("problem", json_value) -> 
      query_item_set_problem v (Pbrt_yojson.string json_value "query_item" "problem")
    | ("result", json_value) -> 
      query_item_set_result v (Pbrt_yojson.string json_value "query_item" "result")
    | ("expected", json_value) -> 
      query_item_set_expected v (Pbrt_yojson.string json_value "query_item" "expected")
    | ("timeS", json_value) -> 
      query_item_set_time_s v (Pbrt_yojson.float json_value "query_item" "time_s")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    prover = v.prover;
    problem = v.problem;
    result = v.result;
    expected = v.expected;
    time_s = v.time_s;
  } : query_item)

let rec decode_json_query_job_results_result d =
  let v = default_query_job_results_result () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("file", json_value) -> 
      query_job_results_result_set_file v (Pbrt_yojson.string json_value "query_job_results_result" "file")
    | ("total", json_value) -> 
      query_job_results_result_set_total v (Pbrt_yojson.int32 json_value "query_job_results_result" "total")
    | ("page", json_value) -> 
      query_job_results_result_set_page v (Pbrt_yojson.int32 json_value "query_job_results_result" "page")
    | ("perPage", json_value) -> 
      query_job_results_result_set_per_page v (Pbrt_yojson.int32 json_value "query_job_results_result" "per_page")
    | ("isComplete", json_value) -> 
      query_job_results_result_set_is_complete v (Pbrt_yojson.bool json_value "query_job_results_result" "is_complete")
    | ("items", `List l) -> begin
      query_job_results_result_set_items v @@ List.map (function
        | json_value -> (decode_json_query_item json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    file = v.file;
    total = v.total;
    page = v.page;
    per_page = v.per_page;
    is_complete = v.is_complete;
    items = v.items;
  } : query_job_results_result)

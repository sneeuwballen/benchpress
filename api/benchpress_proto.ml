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

type list_jobs_request = unit

type job_entry = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 4 fields *)
  mutable job_id : string;
  mutable status : job_status;
  mutable progress_percent : int32;
  mutable result_file : string;
}

type list_jobs_response = {
  mutable jobs : job_entry list;
}

type active_item = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 3 fields *)
  mutable prover : string;
  mutable file : string;
  mutable running_time : float;
}

type stat = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable name : string;
  mutable value : int32;
}

type progress_report = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 6 fields *)
  mutable uuid : string;
  mutable start_ts : float;
  mutable total_tasks : int32;
  mutable done_tasks : int32;
  mutable active : active_item list;
  mutable finished : bool;
  mutable stats : string;
  mutable stat_l : stat list;
}

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

let default_list_jobs_request : list_jobs_request = ()

let default_job_entry (): job_entry =
{
  _presence=Pbrt.Bitfield.empty;
  job_id="";
  status=default_job_status ();
  progress_percent=0l;
  result_file="";
}

let default_list_jobs_response (): list_jobs_response =
{
  jobs=[];
}

let default_active_item (): active_item =
{
  _presence=Pbrt.Bitfield.empty;
  prover="";
  file="";
  running_time=0.;
}

let default_stat (): stat =
{
  _presence=Pbrt.Bitfield.empty;
  name="";
  value=0l;
}

let default_progress_report (): progress_report =
{
  _presence=Pbrt.Bitfield.empty;
  uuid="";
  start_ts=0.;
  total_tasks=0l;
  done_tasks=0l;
  active=[];
  finished=false;
  stats="";
  stat_l=[];
}


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

let[@inline] job_entry_has_job_id (self:job_entry) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] job_entry_has_status (self:job_entry) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] job_entry_has_progress_percent (self:job_entry) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] job_entry_has_result_file (self:job_entry) : bool = (Pbrt.Bitfield.get self._presence 3)

let[@inline] job_entry_set_job_id (self:job_entry) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.job_id <- x
let[@inline] job_entry_set_status (self:job_entry) (x:job_status) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.status <- x
let[@inline] job_entry_set_progress_percent (self:job_entry) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.progress_percent <- x
let[@inline] job_entry_set_result_file (self:job_entry) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.result_file <- x

let copy_job_entry (self:job_entry) : job_entry =
  { self with job_id = self.job_id }

let make_job_entry 
  ?(job_id:string option)
  ?(status:job_status option)
  ?(progress_percent:int32 option)
  ?(result_file:string option)
  () : job_entry  =
  let _res = default_job_entry () in
  (match job_id with
  | None -> ()
  | Some v -> job_entry_set_job_id _res v);
  (match status with
  | None -> ()
  | Some v -> job_entry_set_status _res v);
  (match progress_percent with
  | None -> ()
  | Some v -> job_entry_set_progress_percent _res v);
  (match result_file with
  | None -> ()
  | Some v -> job_entry_set_result_file _res v);
  _res


let[@inline] list_jobs_response_set_jobs (self:list_jobs_response) (x:job_entry list) : unit =
  self.jobs <- x

let copy_list_jobs_response (self:list_jobs_response) : list_jobs_response =
  { self with jobs = self.jobs }

let make_list_jobs_response 
  ?(jobs=[])
  () : list_jobs_response  =
  let _res = default_list_jobs_response () in
  list_jobs_response_set_jobs _res jobs;
  _res

let[@inline] active_item_has_prover (self:active_item) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] active_item_has_file (self:active_item) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] active_item_has_running_time (self:active_item) : bool = (Pbrt.Bitfield.get self._presence 2)

let[@inline] active_item_set_prover (self:active_item) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.prover <- x
let[@inline] active_item_set_file (self:active_item) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.file <- x
let[@inline] active_item_set_running_time (self:active_item) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.running_time <- x

let copy_active_item (self:active_item) : active_item =
  { self with prover = self.prover }

let make_active_item 
  ?(prover:string option)
  ?(file:string option)
  ?(running_time:float option)
  () : active_item  =
  let _res = default_active_item () in
  (match prover with
  | None -> ()
  | Some v -> active_item_set_prover _res v);
  (match file with
  | None -> ()
  | Some v -> active_item_set_file _res v);
  (match running_time with
  | None -> ()
  | Some v -> active_item_set_running_time _res v);
  _res

let[@inline] stat_has_name (self:stat) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] stat_has_value (self:stat) : bool = (Pbrt.Bitfield.get self._presence 1)

let[@inline] stat_set_name (self:stat) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.name <- x
let[@inline] stat_set_value (self:stat) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.value <- x

let copy_stat (self:stat) : stat =
  { self with name = self.name }

let make_stat 
  ?(name:string option)
  ?(value:int32 option)
  () : stat  =
  let _res = default_stat () in
  (match name with
  | None -> ()
  | Some v -> stat_set_name _res v);
  (match value with
  | None -> ()
  | Some v -> stat_set_value _res v);
  _res

let[@inline] progress_report_has_uuid (self:progress_report) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] progress_report_has_start_ts (self:progress_report) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] progress_report_has_total_tasks (self:progress_report) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] progress_report_has_done_tasks (self:progress_report) : bool = (Pbrt.Bitfield.get self._presence 3)
let[@inline] progress_report_has_finished (self:progress_report) : bool = (Pbrt.Bitfield.get self._presence 4)
let[@inline] progress_report_has_stats (self:progress_report) : bool = (Pbrt.Bitfield.get self._presence 5)

let[@inline] progress_report_set_uuid (self:progress_report) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.uuid <- x
let[@inline] progress_report_set_start_ts (self:progress_report) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.start_ts <- x
let[@inline] progress_report_set_total_tasks (self:progress_report) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.total_tasks <- x
let[@inline] progress_report_set_done_tasks (self:progress_report) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.done_tasks <- x
let[@inline] progress_report_set_active (self:progress_report) (x:active_item list) : unit =
  self.active <- x
let[@inline] progress_report_set_finished (self:progress_report) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 4); self.finished <- x
let[@inline] progress_report_set_stats (self:progress_report) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 5); self.stats <- x
let[@inline] progress_report_set_stat_l (self:progress_report) (x:stat list) : unit =
  self.stat_l <- x

let copy_progress_report (self:progress_report) : progress_report =
  { self with uuid = self.uuid }

let make_progress_report 
  ?(uuid:string option)
  ?(start_ts:float option)
  ?(total_tasks:int32 option)
  ?(done_tasks:int32 option)
  ?(active=[])
  ?(finished:bool option)
  ?(stats:string option)
  ?(stat_l=[])
  () : progress_report  =
  let _res = default_progress_report () in
  (match uuid with
  | None -> ()
  | Some v -> progress_report_set_uuid _res v);
  (match start_ts with
  | None -> ()
  | Some v -> progress_report_set_start_ts _res v);
  (match total_tasks with
  | None -> ()
  | Some v -> progress_report_set_total_tasks _res v);
  (match done_tasks with
  | None -> ()
  | Some v -> progress_report_set_done_tasks _res v);
  progress_report_set_active _res active;
  (match finished with
  | None -> ()
  | Some v -> progress_report_set_finished _res v);
  (match stats with
  | None -> ()
  | Some v -> progress_report_set_stats _res v);
  progress_report_set_stat_l _res stat_l;
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

let rec pp_list_jobs_request fmt (v:list_jobs_request) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_unit fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_job_entry fmt (v:job_entry) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~absent:(not (job_entry_has_job_id v)) ~first:true "job_id" Pbrt.Pp.pp_string fmt v.job_id;
    Pbrt.Pp.pp_record_field ~absent:(not (job_entry_has_status v)) ~first:false "status" pp_job_status fmt v.status;
    Pbrt.Pp.pp_record_field ~absent:(not (job_entry_has_progress_percent v)) ~first:false "progress_percent" Pbrt.Pp.pp_int32 fmt v.progress_percent;
    Pbrt.Pp.pp_record_field ~absent:(not (job_entry_has_result_file v)) ~first:false "result_file" Pbrt.Pp.pp_string fmt v.result_file;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_list_jobs_response fmt (v:list_jobs_response) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "jobs" (Pbrt.Pp.pp_list pp_job_entry) fmt v.jobs;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_active_item fmt (v:active_item) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~absent:(not (active_item_has_prover v)) ~first:true "prover" Pbrt.Pp.pp_string fmt v.prover;
    Pbrt.Pp.pp_record_field ~absent:(not (active_item_has_file v)) ~first:false "file" Pbrt.Pp.pp_string fmt v.file;
    Pbrt.Pp.pp_record_field ~absent:(not (active_item_has_running_time v)) ~first:false "running_time" Pbrt.Pp.pp_float fmt v.running_time;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_stat fmt (v:stat) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~absent:(not (stat_has_name v)) ~first:true "name" Pbrt.Pp.pp_string fmt v.name;
    Pbrt.Pp.pp_record_field ~absent:(not (stat_has_value v)) ~first:false "value" Pbrt.Pp.pp_int32 fmt v.value;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_progress_report fmt (v:progress_report) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~absent:(not (progress_report_has_uuid v)) ~first:true "uuid" Pbrt.Pp.pp_string fmt v.uuid;
    Pbrt.Pp.pp_record_field ~absent:(not (progress_report_has_start_ts v)) ~first:false "start_ts" Pbrt.Pp.pp_float fmt v.start_ts;
    Pbrt.Pp.pp_record_field ~absent:(not (progress_report_has_total_tasks v)) ~first:false "total_tasks" Pbrt.Pp.pp_int32 fmt v.total_tasks;
    Pbrt.Pp.pp_record_field ~absent:(not (progress_report_has_done_tasks v)) ~first:false "done_tasks" Pbrt.Pp.pp_int32 fmt v.done_tasks;
    Pbrt.Pp.pp_record_field ~first:false "active" (Pbrt.Pp.pp_list pp_active_item) fmt v.active;
    Pbrt.Pp.pp_record_field ~absent:(not (progress_report_has_finished v)) ~first:false "finished" Pbrt.Pp.pp_bool fmt v.finished;
    Pbrt.Pp.pp_record_field ~absent:(not (progress_report_has_stats v)) ~first:false "stats" Pbrt.Pp.pp_string fmt v.stats;
    Pbrt.Pp.pp_record_field ~first:false "stat_l" (Pbrt.Pp.pp_list pp_stat) fmt v.stat_l;
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

let rec encode_pb_list_jobs_request (v:list_jobs_request) encoder = 
()

let rec encode_pb_job_entry (v:job_entry) encoder = 
  if job_entry_has_job_id v then (
    Pbrt.Encoder.string v.job_id encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  if job_entry_has_status v then (
    encode_pb_job_status v.status encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  );
  if job_entry_has_progress_percent v then (
    Pbrt.Encoder.int32_as_varint v.progress_percent encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  );
  if job_entry_has_result_file v then (
    Pbrt.Encoder.string v.result_file encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  );
  ()

let rec encode_pb_list_jobs_response (v:list_jobs_response) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_job_entry x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.jobs encoder;
  ()

let rec encode_pb_active_item (v:active_item) encoder = 
  if active_item_has_prover v then (
    Pbrt.Encoder.string v.prover encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  if active_item_has_file v then (
    Pbrt.Encoder.string v.file encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  );
  if active_item_has_running_time v then (
    Pbrt.Encoder.float_as_bits64 v.running_time encoder;
    Pbrt.Encoder.key 3 Pbrt.Bits64 encoder; 
  );
  ()

let rec encode_pb_stat (v:stat) encoder = 
  if stat_has_name v then (
    Pbrt.Encoder.string v.name encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  if stat_has_value v then (
    Pbrt.Encoder.int32_as_varint v.value encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  );
  ()

let rec encode_pb_progress_report (v:progress_report) encoder = 
  if progress_report_has_uuid v then (
    Pbrt.Encoder.string v.uuid encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  if progress_report_has_start_ts v then (
    Pbrt.Encoder.float_as_bits64 v.start_ts encoder;
    Pbrt.Encoder.key 2 Pbrt.Bits64 encoder; 
  );
  if progress_report_has_total_tasks v then (
    Pbrt.Encoder.int32_as_varint v.total_tasks encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  );
  if progress_report_has_done_tasks v then (
    Pbrt.Encoder.int32_as_varint v.done_tasks encoder;
    Pbrt.Encoder.key 4 Pbrt.Varint encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_active_item x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  ) v.active encoder;
  if progress_report_has_finished v then (
    Pbrt.Encoder.bool v.finished encoder;
    Pbrt.Encoder.key 6 Pbrt.Varint encoder; 
  );
  if progress_report_has_stats v then (
    Pbrt.Encoder.string v.stats encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_stat x encoder;
    Pbrt.Encoder.key 14 Pbrt.Bytes encoder; 
  ) v.stat_l encoder;
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

let rec decode_pb_list_jobs_request d =
  match Pbrt.Decoder.key d with
  | None -> ();
  | Some (_, pk) -> 
    Pbrt.Decoder.unexpected_payload "Unexpected fields in empty message(list_jobs_request)" pk

let rec decode_pb_job_entry d =
  let v = default_job_entry () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      job_entry_set_job_id v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "job_entry" 1 pk
    | Some (2, Pbrt.Varint) -> begin
      job_entry_set_status v (decode_pb_job_status d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "job_entry" 2 pk
    | Some (3, Pbrt.Varint) -> begin
      job_entry_set_progress_percent v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "job_entry" 3 pk
    | Some (4, Pbrt.Bytes) -> begin
      job_entry_set_result_file v (Pbrt.Decoder.string d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "job_entry" 4 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : job_entry)

let rec decode_pb_list_jobs_response d =
  let v = default_list_jobs_response () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      list_jobs_response_set_jobs v (List.rev v.jobs);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      list_jobs_response_set_jobs v ((decode_pb_job_entry (Pbrt.Decoder.nested d)) :: v.jobs);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "list_jobs_response" 1 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : list_jobs_response)

let rec decode_pb_active_item d =
  let v = default_active_item () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      active_item_set_prover v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "active_item" 1 pk
    | Some (2, Pbrt.Bytes) -> begin
      active_item_set_file v (Pbrt.Decoder.string d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "active_item" 2 pk
    | Some (3, Pbrt.Bits64) -> begin
      active_item_set_running_time v (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "active_item" 3 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : active_item)

let rec decode_pb_stat d =
  let v = default_stat () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      stat_set_name v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "stat" 1 pk
    | Some (2, Pbrt.Varint) -> begin
      stat_set_value v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "stat" 2 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : stat)

let rec decode_pb_progress_report d =
  let v = default_progress_report () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      progress_report_set_stat_l v (List.rev v.stat_l);
      progress_report_set_active v (List.rev v.active);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      progress_report_set_uuid v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "progress_report" 1 pk
    | Some (2, Pbrt.Bits64) -> begin
      progress_report_set_start_ts v (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "progress_report" 2 pk
    | Some (3, Pbrt.Varint) -> begin
      progress_report_set_total_tasks v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "progress_report" 3 pk
    | Some (4, Pbrt.Varint) -> begin
      progress_report_set_done_tasks v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "progress_report" 4 pk
    | Some (5, Pbrt.Bytes) -> begin
      progress_report_set_active v ((decode_pb_active_item (Pbrt.Decoder.nested d)) :: v.active);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "progress_report" 5 pk
    | Some (6, Pbrt.Varint) -> begin
      progress_report_set_finished v (Pbrt.Decoder.bool d);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "progress_report" 6 pk
    | Some (7, Pbrt.Bytes) -> begin
      progress_report_set_stats v (Pbrt.Decoder.string d);
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "progress_report" 7 pk
    | Some (14, Pbrt.Bytes) -> begin
      progress_report_set_stat_l v ((decode_pb_stat (Pbrt.Decoder.nested d)) :: v.stat_l);
    end
    | Some (14, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "progress_report" 14 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : progress_report)

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

let rec encode_json_list_jobs_request (v:list_jobs_request) = 
Pbrt_yojson.make_unit v

let rec encode_json_job_entry (v:job_entry) = 
  let assoc = ref [] in
  if job_entry_has_job_id v then (
    assoc := ("jobId", Pbrt_yojson.make_string v.job_id) :: !assoc;
  );
  if job_entry_has_status v then (
    assoc := ("status", encode_json_job_status v.status) :: !assoc;
  );
  if job_entry_has_progress_percent v then (
    assoc := ("progressPercent", Pbrt_yojson.make_int (Int32.to_int v.progress_percent)) :: !assoc;
  );
  if job_entry_has_result_file v then (
    assoc := ("resultFile", Pbrt_yojson.make_string v.result_file) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_list_jobs_response (v:list_jobs_response) = 
  let assoc = ref [] in
  assoc := (
    let l = v.jobs |> List.map encode_json_job_entry in
    ("jobs", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_active_item (v:active_item) = 
  let assoc = ref [] in
  if active_item_has_prover v then (
    assoc := ("prover", Pbrt_yojson.make_string v.prover) :: !assoc;
  );
  if active_item_has_file v then (
    assoc := ("file", Pbrt_yojson.make_string v.file) :: !assoc;
  );
  if active_item_has_running_time v then (
    assoc := ("runningTime", Pbrt_yojson.make_string (string_of_float v.running_time)) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_stat (v:stat) = 
  let assoc = ref [] in
  if stat_has_name v then (
    assoc := ("name", Pbrt_yojson.make_string v.name) :: !assoc;
  );
  if stat_has_value v then (
    assoc := ("value", Pbrt_yojson.make_int (Int32.to_int v.value)) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_progress_report (v:progress_report) = 
  let assoc = ref [] in
  if progress_report_has_uuid v then (
    assoc := ("uuid", Pbrt_yojson.make_string v.uuid) :: !assoc;
  );
  if progress_report_has_start_ts v then (
    assoc := ("startTs", Pbrt_yojson.make_string (string_of_float v.start_ts)) :: !assoc;
  );
  if progress_report_has_total_tasks v then (
    assoc := ("totalTasks", Pbrt_yojson.make_int (Int32.to_int v.total_tasks)) :: !assoc;
  );
  if progress_report_has_done_tasks v then (
    assoc := ("doneTasks", Pbrt_yojson.make_int (Int32.to_int v.done_tasks)) :: !assoc;
  );
  assoc := (
    let l = v.active |> List.map encode_json_active_item in
    ("active", `List l) :: !assoc 
  );
  if progress_report_has_finished v then (
    assoc := ("finished", Pbrt_yojson.make_bool v.finished) :: !assoc;
  );
  if progress_report_has_stats v then (
    assoc := ("stats", Pbrt_yojson.make_string v.stats) :: !assoc;
  );
  assoc := (
    let l = v.stat_l |> List.map encode_json_stat in
    ("statL", `List l) :: !assoc 
  );
  `Assoc !assoc

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

let rec decode_json_list_jobs_request d =
Pbrt_yojson.unit d "list_jobs_request" "empty record"

let rec decode_json_job_entry d =
  let v = default_job_entry () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("jobId", json_value) -> 
      job_entry_set_job_id v (Pbrt_yojson.string json_value "job_entry" "job_id")
    | ("status", json_value) -> 
      job_entry_set_status v ((decode_json_job_status json_value))
    | ("progressPercent", json_value) -> 
      job_entry_set_progress_percent v (Pbrt_yojson.int32 json_value "job_entry" "progress_percent")
    | ("resultFile", json_value) -> 
      job_entry_set_result_file v (Pbrt_yojson.string json_value "job_entry" "result_file")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    job_id = v.job_id;
    status = v.status;
    progress_percent = v.progress_percent;
    result_file = v.result_file;
  } : job_entry)

let rec decode_json_list_jobs_response d =
  let v = default_list_jobs_response () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("jobs", `List l) -> begin
      list_jobs_response_set_jobs v @@ List.map (function
        | json_value -> (decode_json_job_entry json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    jobs = v.jobs;
  } : list_jobs_response)

let rec decode_json_active_item d =
  let v = default_active_item () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("prover", json_value) -> 
      active_item_set_prover v (Pbrt_yojson.string json_value "active_item" "prover")
    | ("file", json_value) -> 
      active_item_set_file v (Pbrt_yojson.string json_value "active_item" "file")
    | ("runningTime", json_value) -> 
      active_item_set_running_time v (Pbrt_yojson.float json_value "active_item" "running_time")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    prover = v.prover;
    file = v.file;
    running_time = v.running_time;
  } : active_item)

let rec decode_json_stat d =
  let v = default_stat () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("name", json_value) -> 
      stat_set_name v (Pbrt_yojson.string json_value "stat" "name")
    | ("value", json_value) -> 
      stat_set_value v (Pbrt_yojson.int32 json_value "stat" "value")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    name = v.name;
    value = v.value;
  } : stat)

let rec decode_json_progress_report d =
  let v = default_progress_report () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("uuid", json_value) -> 
      progress_report_set_uuid v (Pbrt_yojson.string json_value "progress_report" "uuid")
    | ("startTs", json_value) -> 
      progress_report_set_start_ts v (Pbrt_yojson.float json_value "progress_report" "start_ts")
    | ("totalTasks", json_value) -> 
      progress_report_set_total_tasks v (Pbrt_yojson.int32 json_value "progress_report" "total_tasks")
    | ("doneTasks", json_value) -> 
      progress_report_set_done_tasks v (Pbrt_yojson.int32 json_value "progress_report" "done_tasks")
    | ("active", `List l) -> begin
      progress_report_set_active v @@ List.map (function
        | json_value -> (decode_json_active_item json_value)
      ) l;
    end
    | ("finished", json_value) -> 
      progress_report_set_finished v (Pbrt_yojson.bool json_value "progress_report" "finished")
    | ("stats", json_value) -> 
      progress_report_set_stats v (Pbrt_yojson.string json_value "progress_report" "stats")
    | ("statL", `List l) -> begin
      progress_report_set_stat_l v @@ List.map (function
        | json_value -> (decode_json_stat json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    uuid = v.uuid;
    start_ts = v.start_ts;
    total_tasks = v.total_tasks;
    done_tasks = v.done_tasks;
    active = v.active;
    finished = v.finished;
    stats = v.stats;
    stat_l = v.stat_l;
  } : progress_report)

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
    open Pbrt_services
    
    let listJobs : (list_jobs_request, unary, list_jobs_response, unary) Client.rpc =
      (Client.mk_rpc 
        ~package:["benchpress_api"]
        ~service_name:"BenchpressApi" ~rpc_name:"ListJobs"
        ~req_mode:Client.Unary
        ~res_mode:Client.Unary
        ~encode_json_req:encode_json_list_jobs_request
        ~encode_pb_req:encode_pb_list_jobs_request
        ~decode_json_res:decode_json_list_jobs_response
        ~decode_pb_res:decode_pb_list_jobs_response
        () : (list_jobs_request, unary, list_jobs_response, unary) Client.rpc)
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
    
    let listJobs : (list_jobs_request,unary,list_jobs_response,unary) Server.rpc = 
      (Server.mk_rpc ~name:"ListJobs"
        ~req_mode:Server.Unary
        ~res_mode:Server.Unary
        ~encode_json_res:encode_json_list_jobs_response
        ~encode_pb_res:encode_pb_list_jobs_response
        ~decode_json_req:decode_json_list_jobs_request
        ~decode_pb_req:decode_pb_list_jobs_request
        () : _ Server.rpc)
    
    let make
      ~newJob:__handler__newJob
      ~getJobStatus:__handler__getJobStatus
      ~cancelJob:__handler__cancelJob
      ~listJobs:__handler__listJobs
      () : _ Server.t =
      { Server.
        service_name="BenchpressApi";
        package=["benchpress_api"];
        handlers=[
           (__handler__newJob newJob);
           (__handler__getJobStatus getJobStatus);
           (__handler__cancelJob cancelJob);
           (__handler__listJobs listJobs);
        ];
      }
  end
  
end

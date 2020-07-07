
(** {1 Task queue for the server} *)

module Fmt = CCFormat
module M = CCLock

let src_log = Logs.Src.create "task-queue"
let timeout_api_expire_s = 60. (* after which api jobs expire *)

type job = {
  j_uuid: string;
  j_action: Action.t;
  j_task: Task.t; (* task this action comes from *)
  j_interrupted: bool M.t;
  mutable j_started_time: float; (* -1. if not started *)
  mutable j_percent_completion: int;
  mutable j_eta: float;
}

module Job = struct
  type t = job

  let task self = self.j_task
  let pp out self =
    Fmt.fprintf out "(@[task%s@ :uuid %s@ %a@])"
      (if M.get self.j_interrupted then "[int]" else "")
      self.j_uuid Action.pp self.j_action
  let uuid self = self.j_uuid
  let to_string = Fmt.to_string pp
  let interrupt self = M.set self.j_interrupted true
  let interrupted self = M.get self.j_interrupted
  let time_elapsed self = Unix.gettimeofday() -. self.j_started_time
  let started self = self.j_started_time > -1.
  let set_percent_completion self i = self.j_percent_completion <- i
  let set_eta self f = self.j_eta <- f

  let to_api_descr self =
    let t_id = self.j_uuid in
    let t_descr = Task.to_string self.j_task in
    let t_status = if started self then (
        Api.T_in_progress {Api.time_elapsed=time_elapsed self; estimated_completion=0l}
      ) else Api.T_waiting
    in
    {Api.t_id; t_descr; t_status }
end

(* TODO: replace the blocking queue with a custom thing with priorities *)

type api_job = {
  mutable aj_last_seen: float;
  mutable aj_interrupted: bool;
  mutable aj_descr: Api.task_descr;
}

type t = {
  defs: Definitions.t M.t;
  jobs: job CCBlockingQueue.t;
  jobs_tbl: (string, Job.t) Hashtbl.t;
  api_jobs: (string, api_job) Hashtbl.t; (* last seen+descr *)
  cur: job option M.t;
}

let defs self = self.defs
let size self = CCBlockingQueue.size self.jobs
let cur_job self = M.get self.cur

let interrupt self ~uuid : bool =
  match CCHashtbl.get self.jobs_tbl uuid, CCHashtbl.get self.api_jobs uuid with
  | Some j, _ -> M.set j.j_interrupted true; true
  | None, Some aj -> aj.aj_interrupted <- true; true
  | None, None -> false

let create ?(defs=Definitions.empty) () : t =
  { jobs= CCBlockingQueue.create 64;
    defs=M.create defs;
    jobs_tbl=Hashtbl.create 8;
    api_jobs=Hashtbl.create 8;
    cur=M.create None;
  }

let push self task : unit =
  let j_uuid =
    Uuidm.v4_gen (Random.State.make_self_init()) ()
    |> Uuidm.to_string
  in
  let j = {
    j_action=task.Task.action; j_task=task; j_uuid; j_eta=0.;
    j_interrupted=M.create false; j_started_time= -1. ; j_percent_completion=0;
  } in
  Hashtbl.add self.jobs_tbl j_uuid j;
  CCBlockingQueue.push self.jobs j

let loop self =
  while true do
    let job = CCBlockingQueue.take self.jobs in
    job.j_started_time <- Unix.gettimeofday();
    M.set self.cur (Some job);
    Logs.info ~src:src_log (fun k->k "run job for task %s" job.j_task.Task.name);
    let defs = M.get self.defs in
    (* run the job *)
    begin
      let cb_progress = object
        method on_progress ~percent ~elapsed_time:_ ~eta =
          job.j_percent_completion <- percent;
          job.j_eta <- eta;
        method on_done = ()
      end in
      match
        Exec_action.run defs job.j_action ~cb_progress
          ~interrupted:(fun () -> Job.interrupted job)
      with
      | Ok () -> ()
      | Error e ->
        Logs.err ~src:src_log
          (fun k->k "error while running job %s: %s" job.j_task.Task.name e);
      | exception e ->
        Logs.err ~src:src_log
          (fun k->k "error while running job %s: %s"
              job.j_task.Task.name (Printexc.to_string e));
    end;
    Hashtbl.remove self.jobs_tbl job.j_uuid; (* cleanup *)
    M.set self.cur None;
  done

let api_update_external_job self d =
  let uuid = d.Api.t_id in
  match d.Api.t_status with
  | Api.T_in_progress _ | Api.T_waiting ->
    Logs.debug (fun k->k"task-queue: update external job `%s`" uuid);
    let aj =
      try Hashtbl.find self.api_jobs uuid
      with Not_found ->
        let aj = {aj_descr=d; aj_last_seen=0.; aj_interrupted=false} in
        Hashtbl.add self.api_jobs uuid aj;
        aj
    in
    aj.aj_last_seen <- Unix.gettimeofday();
    aj.aj_descr <- d;
    if aj.aj_interrupted then `Interrupted else `Ok
  | Api.T_done ->
    Hashtbl.remove self.api_jobs uuid;
    `Ok

let api_task_list self : Api.task_list =
  let active, waiting =
    CCHashtbl.to_list self.jobs_tbl
    |> List.rev_map snd
    |> List.partition Job.started
  in
  let active = ref (List.map Job.to_api_descr active) in
  let waiting = ref (List.map Job.to_api_descr waiting) in
  Hashtbl.iter
    (fun _ aj ->
       let t = aj.aj_descr in
       if aj.aj_last_seen +. timeout_api_expire_s < Unix.gettimeofday() then (
         Hashtbl.remove self.api_jobs t.Api.t_id; (* timeout *)
       ) else (
         match t.Api.t_status with
         | Api.T_in_progress _ -> active := t :: !active
         | _ -> waiting := t :: !waiting
       ))
    self.api_jobs;
  {Api.active= !active; waiting= !waiting}


module Basic_status = struct
  module J = Misc.Json
  type json = J.t
  type t = Api.task_list

  let task_to_j (t:Api.task_descr) : J.t =
    let l = List.flatten @@ [
        [ "uuid", `String t.Api.t_id;
          "descr", `String t.Api.t_descr;
        ];
        (match t.Api.t_status with
         | Api.T_waiting -> ["status", `String "waiting"]
         | Api.T_done -> ["status", `String "done"]
         | Api.T_in_progress {time_elapsed=t; estimated_completion=c} ->
           ["status", `String "done";
            "time_elapsed", `String (Misc.human_duration t);
            "estimated_completion", `Int (Int32.to_int c)
           ])
      ]
    in
    `Assoc l

  let to_json (self:t) =
    let j: json = `Assoc [
        "active", `List (List.map task_to_j self.active);
        "waiting", `List (List.map task_to_j self.waiting);
      ] in
    J.to_string j
end

let basic_status = api_task_list

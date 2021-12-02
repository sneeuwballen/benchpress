
(** {1 Task queue for the server} *)

open Common
module M = CCLock

module Log = (val Logs.src_log (Logs.Src.create "benchpress.task-queue"))

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
end

(* TODO: replace the blocking queue with a custom thing with priorities *)

type api_job = {
  mutable aj_last_seen: float;
  mutable aj_interrupted: bool;
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
    Profile.with_ "task-queue.job" @@ fun () ->
    job.j_started_time <- Unix.gettimeofday();
    M.set self.cur (Some job);
    Log.info (fun k->k "run job for task %s" job.j_task.Task.name);
    let defs = M.get self.defs in
    (* run the job *)
    begin
      let cb_progress = object
        method on_progress ~percent ~elapsed_time:_ ~eta =
          job.j_percent_completion <- percent;
          job.j_eta <- eta;
        method on_done = ()
      end in
      try
        Exec_action.run defs job.j_action ~cb_progress
          ~interrupted:(fun () -> Job.interrupted job)
      with
      | Error.E e ->
        Log.err
          (fun k->k "error while running job %s:@ %a" job.j_task.Task.name Error.pp e);
      | e ->
        Log.err
          (fun k->k "error while running job %s:@ %s"
              job.j_task.Task.name (Printexc.to_string e));
    end;
    Hashtbl.remove self.jobs_tbl job.j_uuid; (* cleanup *)
    M.set self.cur None;
  done

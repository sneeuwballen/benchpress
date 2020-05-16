
(** {1 Task queue for the server} *)

module Fmt = CCFormat
module M = CCLock

let src_log = Logs.Src.create "task-queue"

type job = {
  j_action: Action.t;
  j_task: Task.t; (* task this action comes from *)
  j_interrupted: bool M.t;
  mutable j_started_time: float;
}
module Job = struct
  type t = job

  let task self = self.j_task
  let pp out self =
    Fmt.fprintf out "(@[task%s %a@])"
      (if M.get self.j_interrupted then "[int]" else "")
      Action.pp self.j_action
  let to_string = Fmt.to_string pp
  let interrupt self = M.set self.j_interrupted true
  let interrupted self = M.get self.j_interrupted
  let time_elapsed self = Unix.gettimeofday() -. self.j_started_time
end

(* TODO: replace the blocking queue with a custom thing with priorities *)

type t = {
  defs: Definitions.t M.t;
  jobs: job CCBlockingQueue.t;
  cur: job option M.t;
}

let defs self = self.defs
let size self = CCBlockingQueue.size self.jobs
let cur_job self = M.get self.cur

let create ?(defs=Definitions.empty) () : t =
  { jobs= CCBlockingQueue.create 200;
    defs=M.create defs;
    cur=M.create None;
  }

let push self task : unit =
  let j = {
    j_action=task.Task.action; j_task=task;
    j_interrupted=M.create false; j_started_time=0. ;
  } in
  CCBlockingQueue.push self.jobs j

let loop self =
  while true do
    let job = CCBlockingQueue.take self.jobs in
    job.j_started_time <- Unix.gettimeofday();
    M.set self.cur (Some job);
    Logs.info ~src:src_log (fun k->k "run job for task %s" job.j_task.Task.name);
    let defs = M.get self.defs in
    (* run the job *)
    begin match
        Exec_action.run defs job.j_action
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
    M.set self.cur None;
  done

type status = {
  cur_job: Job.t option;
  in_queue: int;
}

let status self =
  {cur_job=M.get self.cur; in_queue=CCBlockingQueue.size self.jobs}

let status_to_json st =
  let spf = Printf.sprintf in
  let cj = match st.cur_job with
    | None -> "null"
    | Some j ->
      spf {| { "task": %S, "elapsed": %0.3f } |}
        (Job.to_string j) (Job.time_elapsed j)
  in
  spf {| { "cur_job": %s, "in_queue": %d } |} cj st.in_queue

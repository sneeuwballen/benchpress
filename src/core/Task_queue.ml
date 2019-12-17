
(** {1 Task queue for the server} *)

module Fmt = CCFormat
module M = CCLock

let src_log = Logs.Src.create "task-queue"

type job = {
  j_action: Action.t;
  j_task: Task.t; (* task this action comes from *)
  j_interrupted: bool M.t;
}
module Job = struct
  type t = job

  let task self = self.j_task
  let pp out self =
    Fmt.fprintf out "(@[task%s %a@])"
      (if M.get self.j_interrupted then "[int]" else "")
      Action.pp self.j_action
  let interrupt self = M.set self.j_interrupted true
  let interrupted self = M.get self.j_interrupted
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
    j_interrupted=M.create false;
  } in
  CCBlockingQueue.push self.jobs j

let loop self =
  while true do
    let job = CCBlockingQueue.take self.jobs in
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



module Fmt = CCFormat

type task_progress = {
  time_elapsed : float; (* in seconds *)
  estimated_completion : int; (* in percent *)
}

type task_status =
  | T_waiting
  | T_in_progress of task_progress
  | T_done

type task_descr = {
  t_id : string;
  t_descr : string;
  t_status : task_status;
}

type task_list = {
  active: task_descr list;
  waiting: task_descr list;
}

type ok_or_interrupted_res =
  | R_ok
  | R_interrupted

let pp_task_progress out (v:task_progress) =
  Fmt.fprintf out "{@[task_progress=%.2fs;@ completion=%d%%@]}"
    v.time_elapsed v.estimated_completion

let pp_task_status out= function
  | T_waiting  -> Format.fprintf out "T_waiting"
  | T_in_progress x -> Format.fprintf out "(@[T_in_progress %a@])" pp_task_progress x
  | T_done  -> Format.fprintf out "T_done"

let pp_task_descr out (v:task_descr) =
  Fmt.fprintf out "{@[id=%s;@ descr=%S;@ status=@[%a@]@]}"
    v.t_id v.t_descr pp_task_status v.t_status

let pp_task_list out (v:task_list) =
  Fmt.fprintf out "{@[active=%a;@ waiting=%a@]}"
    (Fmt.Dump.list pp_task_descr) v.active
    (Fmt.Dump.list pp_task_descr) v.waiting

let pp_ok_or_interrupted out (v:ok_or_interrupted_res) =
  match v with
  | R_ok -> Format.fprintf out "Ok"
  | R_interrupted -> Format.fprintf out "Interrupted"

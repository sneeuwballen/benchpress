type master_msg =
  | Worker_task of (string * Problem.t) list (* prover name, problem *)
  | Stop_worker

type worker_msg =
  | Worker_response of int * Run_event.t list
  | Worker_failure of int * exn

type th_msg = Work_done

(** Structured progress tracking for benchpress jobs.

    A {!t} is a handler that receives events from a running job and forwards
    them to callbacks as {!report} values.

    Multiple handlers can be composed with {!fanout} (e.g. terminal bar + NATS
    publisher + HTTP callback). *)

module Api = Benchpress_api_proto.Benchpress_api

type report = Api.progress_report
(** Full progress snapshot for a job. *)

type summary = { percent: int; eta: float }
(** Quick summary extracted from a report. [eta] is [infinity] if unknown. *)

val summarize : report -> summary
(** Extract percent complete and estimated time remaining. *)

class type t = object
  method on_res : Run_prover_problem.job_res -> unit
  method on_start_proof_check : unit
  method on_proof_check_res : Test.proof_check_result -> unit
  method on_done : unit
end

type callbacks = {
  on_report: report -> unit;
  on_done: unit -> unit;
  on_res: Run_prover_problem.job_res -> unit;
}

class nil : t
(** No-op tracker. *)

val fanout : t list -> t
(** Dispatch every event to all trackers in the list. *)

val make :
  uuid:string -> start_ts:float -> total_tasks:int -> callbacks:callbacks -> t
(** Create a progress tracker for a job.

    Maintains internal state (done count, active items, result stats) and calls
    [callbacks.on_report] with a full {!report} on every event.
    [callbacks.on_done] is called once when the job finishes. [callbacks.on_res]
    is called on each individual solve result. *)

val make_terminal : total_tasks:int -> ?pp_results:bool -> unit -> t
(** Create a tracker that prints a progress bar and individual results to
    stdout. Compatible with the traditional CLI progress bar. *)


(** {1 Task queue for the server} *)

module Fmt = CCFormat

type 'a or_error = ('a, string) result

module Job : sig
  type t

  val uuid : t -> string
  val task : t -> Task.t
  val interrupt : t -> unit
  val pp : t Fmt.printer
  val to_string : t -> string

  val time_elapsed: t -> float
end

type t

val create : ?defs:Definitions.t -> unit -> t

val defs : t -> Definitions.t CCLock.t
(** List of definitions available for tasks *)

val size : t -> int

val cur_job : t -> Job.t option
(** Current job, if any *)

val push : t -> Task.t -> unit
(** Push a task to execute *)
(* TODO: priorities, so that some tasks are more urgent *)

val loop : t -> unit
(** Run forever *)

val interrupt : t -> uuid:string -> bool
(** Interrupt task. Returns true on success *)

val api_update_external_job : t -> Api.task_descr -> [`Ok | `Interrupted]

val api_task_list : t -> Api.task_list or_error

module Basic_status : sig
  type t

  val to_json : t -> string
end

val basic_status : t -> Basic_status.t or_error

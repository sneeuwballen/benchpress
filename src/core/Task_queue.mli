
(** {1 Task queue for the server} *)

module Fmt = CCFormat

module Job : sig
  type t

  val task : t -> Task.t
  val interrupt : t -> unit
  val pp : t Fmt.printer
  val to_string : t -> string

  val time_elapsed: t -> float

  (* TODO: completion, in [0;1]
     val completion : t -> float
  *)
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

type status = {
  cur_job: Job.t option;
  in_queue: int;
}

val status : t -> status

val status_to_json : status -> string

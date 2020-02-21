
(** {1 API context} *)

module P = Protocol_t

type t

val create :
  list_tasks:(unit -> P.task_descr option * int) ->
  unit -> t

val push_notification : t -> P.push_notification Signal.t

val list_tasks : t -> P.task_descr option * int

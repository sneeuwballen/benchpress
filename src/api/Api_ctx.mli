
(** {1 API context} *)

module P = Protocol_t

type t

val create : unit -> t

val push_notification : t -> P.push_notification Signal.t

(** Human-readable formatting utilities *)

val human_duration : float -> string
(** Format a float duration in seconds to a readable string *)

val pp_human_duration : Format.formatter -> float -> unit
(** Pretty-print a duration *)

val human_datetime : float -> string
(** Format a Unix timestamp to a human-readable datetime string *)

val pp_human_datetime : Format.formatter -> float -> unit
(** Pretty-print a datetime *)

val datetime_compact : Ptime.t -> string
(** Compact representation of a datetime (YYYYMMDDThhmmss) *)

val human_size : int -> string
(** Format a byte count to a human-readable size string *)

val truncate_left : int -> string -> string
(** Truncate a string on the left to a maximum length *)

val truncate_right : int -> string -> string
(** Truncate a string on the right to a maximum length *)

(** Timer for measuring elapsed time *)
module Chrono : sig
  type t

  val start : unit -> t
  val since_last : t -> float
  val elapsed : t -> float
end

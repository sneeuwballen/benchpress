(** Human-readable formatting utilities *)

(** Format a float duration in seconds to a readable string *)
val human_duration : float -> string

(** Pretty-print a duration *)
val pp_human_duration : Format.formatter -> float -> unit

(** Format a Unix timestamp to a human-readable datetime string *)
val human_datetime : float -> string

(** Pretty-print a datetime *)
val pp_human_datetime : Format.formatter -> float -> unit

(** Compact representation of a datetime (YYYYMMDDThhmmss) *)
val datetime_compact : Ptime.t -> string

(** Format a byte count to a human-readable size string *)
val human_size : int -> string

(** Truncate a string on the left to a maximum length *)
val truncate_left : int -> string -> string

(** Truncate a string on the right to a maximum length *)
val truncate_right : int -> string -> string

(** Timer for measuring elapsed time *)
module Chrono : sig
  type t

  val start : unit -> t
  val since_last : t -> float
  val elapsed : t -> float
end

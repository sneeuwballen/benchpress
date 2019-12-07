(* This file is free software. See file "license" for more details. *)

(** {1 Notifications} *)

type t

val send : t -> string -> unit
val sendf : t -> ('a, Format.formatter, unit, unit) format4 -> 'a

val nil : t

val stdout : t

val combine : t -> t -> t
val combine_l : t list -> t

val make : Definitions.t -> t
(** Make a combination of notification systems *)

val sync : t -> unit
(** Wait for notifications to be done *)

(** Lightweight JSON type and printer *)

type t =
  [ `String of string
  | `List of t list
  | `Assoc of (string * t) list
  | `Int of int
  | `Float of float
  | `Null ]

(** Pretty-print JSON *)
val pp : Format.formatter -> t -> unit

(** Convert JSON to a string *)
val to_string : t -> string

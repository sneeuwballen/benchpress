(* This file is free software. See file "license" for more details. *)

(** {1 Result of a single job} *)

type t =
  | Sat
  | Unsat
  | Unknown
  | Timeout
  | Error
  | Tag of string

val compare: t -> t -> [`Same | `LeftBetter | `RightBetter | `Mismatch]
(** [compare a b] compares results [a] and [b] (assuming they are results
    of two distinct provers on the same problem), and returns:

    {ul
      {- `Same if results coincide}
      {- `Mismatch if they are not compatible (error)}
      {- `LeftBetter if [b = Unknown] and [a = Sat] or [a = Unsat]}
      {- `RightBetter if [a = Unknown] and [b = Sat] or [b = Unsat]}
    }
*)

val pp : t CCFormat.printer
val to_string : t -> string
val of_string : ?tags:string list -> string -> t


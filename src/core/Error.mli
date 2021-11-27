
(** Main error type *)

open Common

type t

exception E of t

val raise : t -> 'a

val make : ?loc:Loc.t -> string -> t
val makef : ?loc:Loc.t -> ('a, Format.formatter, unit, t) format4 -> 'a
val of_exn : ?loc:Loc.t -> exn -> t

val wrap : ?loc:Loc.t -> string -> t -> t
val wrapf : ?loc:Loc.t -> ('a, Format.formatter, unit, t -> t) format4 -> 'a

val msg : t -> string
val ctx_of : t -> t option
val unwrap_ctx : t -> t * t list
val loc : t -> Loc.t option

val pp : t Fmt.printer
val show : t -> string

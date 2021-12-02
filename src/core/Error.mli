
(** Main error type *)

open Common

type t

exception E of t

val raise : t -> 'a

val make : ?loc:Loc.t -> string -> t
val makef : ?loc:Loc.t -> ('a, Format.formatter, unit, t) format4 -> 'a
val of_exn : ?loc:Loc.t -> exn -> t

val wrap : ?loc:Loc.t -> string -> t -> t
(** [wrap msg e] makes a composite error that gives context to [e]. *)

val wrapf : ?loc:Loc.t -> ('a, Format.formatter, unit, t -> t) format4 -> 'a

val msg : t -> string
val ctx_of : t -> t option
val unwrap_ctx : t -> t * t list
val loc : t -> Loc.t option

val unwrap : ('a, t) result -> 'a
(** Obtain result, or {!raise} error *)

val unwrap_opt : ?loc:Loc.t -> string -> 'a option -> 'a
val unwrap_opt' : ?loc:Loc.t -> (unit -> string) -> 'a option -> 'a

val fail : ?loc:Loc.t -> string -> 'a
(** Build error and {!raise} it *)

val failf : ?loc:Loc.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val guard : (t -> t) -> (unit -> 'a) -> 'a
(** [guard wrap f] runs [f()], and wraps the error with [wrap] if
    it fails.

    Typical usage: [Error.guard (Error.wrapf "oh no %d" 42) @@ fun () -> …]
*)

val pp : t Fmt.printer
val show : t -> string

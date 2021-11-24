
open Common

module Input : sig
  type t
  val string : string -> t
  val file : string -> t
end

type pos = {line: int; col: int}

module Pos : sig
  type t = pos
  val (<=) : t -> t -> bool
  val (<) : t -> t -> bool
  val (=) : t -> t -> bool
  val min : t -> t -> t
  val max : t -> t -> t
  val pp : t Fmt.printer
end

type t = {
  file: string;
  start: pos;
  stop: pos;
  input: Input.t;
}

val none : t
val pp : t Fmt.printer
val pp_l : t list Fmt.printer

val contains : t -> pos -> bool

val union : t -> t -> t
val union_l : t list -> t option

val of_lexbuf : input:Input.t -> Lexing.lexbuf -> t


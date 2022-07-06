
open Pp_loc

type pos = Position.t

module Input : sig
  type t
  val string : string -> t
  val file : string -> t
end

module Pos : sig
  val dummy : pos
  val of_line_col : int -> int -> pos
  val to_line_col : Input.t -> pos -> int * int
  val to_lexing : ?filename:string -> Input.t -> pos -> Lexing.position
  val le : Input.t -> pos -> pos -> bool
end

type t = {
  start: pos;
  stop: pos;
  input: Input.t;
}

val none : t

val loc : t -> loc

val contains : t -> pos -> bool

val of_lexbuf : input:Input.t -> Lexing.lexbuf -> t

val pp : Format.formatter -> t -> unit


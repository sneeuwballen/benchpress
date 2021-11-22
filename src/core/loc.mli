
open Common

module Input : sig
  type t
  val string : string -> t
  val file : string -> t
end

type pos = {line: int; col: int}

type t = {
  file: string;
  start: pos;
  stop: pos;
  input: Input.t;
}

val none : t
val pp : t Fmt.printer
val pp_l : t list Fmt.printer

val of_lexbuf : input:Input.t -> Lexing.lexbuf -> t

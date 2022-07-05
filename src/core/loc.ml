
open Pp_loc

type pos = Position.t

module Input = struct
  type t = Input.t
  let string = Input.string
  let file = Input.file
end

module Pos = struct
  let dummy = Position.of_lexing Lexing.dummy_pos

  let of_line_col = Position.of_line_col

  let to_line_col input p =
    let Lexing.{ pos_lnum; pos_cnum; _ } = Position.to_lexing input p in
    pos_lnum, pos_cnum

  let to_lexing = Position.to_lexing

  let le input (p1: pos) (p2: pos) =
    let al, ac = to_line_col input p1 in
    let bl, bc = to_line_col input p2 in
    al < bl || (al = bl && ac <= bc)

end

type t = {
  start: pos;
  stop: pos;
  input: Input.t;
}

let none = { start = Pos.dummy; stop = Pos.dummy; input = Input.file ""; }

let loc { start; stop; _ } = start, stop

let contains { start; stop; input; } pos =
  Pos.le input start pos &&
  Pos.le input pos stop

let of_lexbuf ~input (lexbuf:Lexing.lexbuf) =
  let start = Position.of_lexing lexbuf.lex_start_p in
  let stop = Position.of_lexing lexbuf.lex_curr_p in
  { start; stop; input; }

let pp fmt { start; stop; input; } =
  Pp_loc.pp ~input fmt [start, stop]


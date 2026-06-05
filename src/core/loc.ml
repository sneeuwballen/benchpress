type pos = { line: int; col: int }
type t = { file: string; start: pos; stop: pos }

let none =
  { file = ""; start = { line = 0; col = 0 }; stop = { line = 0; col = 0 } }

let pp fmt { file; start; stop } =
  if start.line = stop.line then
    if start.col = stop.col then
      Format.fprintf fmt "%s:%d" file start.line
    else
      Format.fprintf fmt "%s:%d:%d-%d" file start.line start.col stop.col
  else
    Format.fprintf fmt "%s:%d:%d-%d:%d" file start.line start.col stop.line
      stop.col

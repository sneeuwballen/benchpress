
open Common

module Index : sig
  type t
  val of_string : string -> t
  val find_line_offset : t -> line:int -> int
  val find_offset : t -> line:int -> col:int -> int
end = struct
  module Vec = CCVector
  (* a list of offsets of newlines *)
  type t = {
    lines: int Vec.ro_vector;
    size: int; (* total length *)
  }

  let of_string (s:string) : t =
    let lines = Vec.create() in
    Vec.push lines 0; (* first line is free *)
    let size = String.length s in
    let i = ref 0 in
    while !i < size do
      match String.index_from_opt s !i '\n' with
      | None -> i := size
      | Some j ->
        Vec.push lines j;
        i := j+1;
    done;
    let lines = Vec.freeze lines in
    { lines; size; }

  let find_line_offset (self:t) ~line : int =
    let line = line-1 in
    if line >= Vec.length self.lines then (
      self.size
    ) else (
      Vec.get self.lines line
    )

  let find_offset (self:t) ~line ~col : int =
    let off = find_line_offset self ~line in
    off + (col - 1)
end

module Input = struct
  type view =
    | String of string
    | File of string lazy_t

  type t = {
    view: view;
    idx: Index.t lazy_t;
  }

  let string s : t = {
    view=String s;
    idx=lazy (Index.of_string s);
  }

  let file file : t =
    let view = lazy (
      Printf.printf "reading %s\n%!" file;
      CCIO.File.read_exn file) in
    let idx = lazy (Index.of_string (Lazy.force view)) in
    { view=File view; idx; }

  let to_pp_loc_input (self:t) =
    match self.view with
    | String s -> Pp_loc.Input.string s
    | File (lazy s) -> Pp_loc.Input.string s

  let find_line_offset (self:t) ~line : int =
    Index.find_line_offset (Lazy.force self.idx) ~line

  let find_offset (self:t) ~line ~col : int =
    Index.find_offset (Lazy.force self.idx) ~line ~col
end

type pos = {line: int; col: int}

type t = {
  file: string;
  start: pos;
  stop: pos;
  input: Input.t;
}

let none = {file="<none>"; input=Input.string "";
            start={line=1;col=1}; stop={line=1;col=1}}

module Pos = struct
  type t = pos
  let (<=) a b = a.line < b.line || (a.line = b.line && a.col <= b.col)
  let (<) a b = a.line < b.line || (a.line = b.line && a.col < b.col)
  let (=) a b = a.line = b.line && a.col = b.col
  let min a b = if a<=b then a else b
  let max a b = if a<=b then b else a

  let pp out self = Fmt.fprintf out "line %d, col %d" self.line self.col
end

let contains loc pos =
  Pos.( loc.start <= pos && pos <= loc.stop )

let tr_position (self:t) (pos:pos) : Lexing.position =
  let line_offset = Input.find_line_offset self.input ~line:pos.line in
  {Lexing.pos_fname=self.file;
   pos_lnum=pos.line;
   pos_cnum=line_offset + pos.col + 1;
   pos_bol=line_offset;
  }

let tr_loc (self:t) : Pp_loc.loc =
  tr_position self self.start,
  tr_position self self.stop

let pp_compact out (self:t) =
  if self.start.line=self.stop.line then (
    Format.fprintf out "In file '%s', line %d columns %d..%d"
      self.file self.start.line self.start.col self.stop.col
  ) else (
    Format.fprintf out "In file '%s', line %d col %d to line %d col %d"
      self.file self.start.line self.start.col self.stop.line self.stop.col
  )

let pp out (self:t) : unit =
  let input = Input.to_pp_loc_input self.input in
  Format.fprintf out "@[<v>%a@ %a@]"
    pp_compact self
    (Pp_loc.pp ~max_lines:5 ~input) [tr_loc self]

let pp_l out (l:t list) : unit =
  if l=[] then ()
  else (
    let input = Input.to_pp_loc_input (List.hd l).input in
    let locs = List.map tr_loc l in
    Format.fprintf out "@[<v>%a@ %a@]"
      Fmt.(list ~sep:(return ";@ and ") pp_compact) l
      (Pp_loc.pp ~max_lines:5 ~input) locs
  )

let of_lexbuf ~input (lexbuf:Lexing.lexbuf) : t =
  let open Lexing in
  let start = lexbuf.lex_start_p in
  let stop = lexbuf.lex_curr_p in
  let file = start.pos_fname in
  let tr_pos p = {line=p.pos_lnum; col=p.pos_cnum - p.pos_bol + 1} in
  {file; input; start=tr_pos start; stop=tr_pos stop}

let union a b =
  {start=Pos.min a.start b.start;
   stop=Pos.max a.stop b.stop;
   file=a.file; input=a.input}

let union_l = function
  | [] -> None
  | [l] -> Some l
  | l1 :: tl -> Some (List.fold_left union l1 tl)

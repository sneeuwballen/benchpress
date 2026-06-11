(** {1 Directory of problems} *)

module Fmt = CCFormat

type regex = string
(** A regex in Perl syntax *)

type expect =
  | E_comment  (** look in comments *)
  | E_const of Res.t
  | E_program of { prover: Prover.t }
  | E_try of expect list  (** Try these methods successively *)

type t = {
  name: string option;
  path: string;
  expect: expect;
  pattern: regex option;  (** Pattern of problems in this directory *)
  loc: Loc.t;
}

let rec pp_expect out = function
  | E_comment -> Fmt.string out "comment"
  | E_const r -> Fmt.fprintf out "const: %a" Res.pp r
  | E_program { prover } -> Fmt.fprintf out "run: %s" (Prover.name prover)
  | E_try [] -> Fmt.string out "try: []"
  | E_try l ->
    Format.fprintf out "@[<v2>try:";
    List.iter (fun e -> Format.fprintf out "@,- %a" pp_expect e) l;
    Format.fprintf out "@]"

let pp out { name; path; expect; pattern; loc = _ } : unit =
  let open Misc.Pp in
  pp_record "dir" out
    [
      field_opt "name" Fmt.string name;
      field "path" Fmt.string path;
      field "expect" pp_expect expect;
      field_opt "pattern" pp_regex pattern;
    ]

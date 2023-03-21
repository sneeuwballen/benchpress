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
  path: string;
  expect: expect;
  pattern: regex option;  (** Pattern of problems in this directory *)
  loc: Loc.t;
}

let rec pp_expect out = function
  | E_comment -> Fmt.string out "comments"
  | E_const r -> Fmt.fprintf out "(@[const %a@])" Res.pp r
  | E_program { prover } -> Fmt.fprintf out "(@[run %a@])" Prover.pp_name prover
  | E_try l -> Fmt.fprintf out "(@[try@ %a@])" (Misc.pp_list pp_expect) l

let pp out { path; expect; pattern; loc = _ } : unit =
  let open Misc.Pp in
  Fmt.fprintf out "(@[<v1>dir%a%a%a@])" (pp_f "path" Fmt.string) path
    (pp_f "expect" pp_expect) expect
    (pp_opt "pattern" pp_regex)
    pattern

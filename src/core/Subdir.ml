module Fmt = CCFormat

type t = { path: string; inside: Dir.t; loc: Loc.t }
(** A directory of problems, inside a known set of problems which defines what
    patterns to search for, and how to read "expect" results. *)

let pp out (self : t) =
  let open Misc.Pp in
  pp_record "dir" out
    [
      field "path" pp_str self.path;
      field_opt "name" Fmt.string self.inside.Dir.name;
      field_opt "pattern" pp_regex self.inside.Dir.pattern;
    ]

let pattern self = self.inside.Dir.pattern
let expect self = self.inside.Dir.expect

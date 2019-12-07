
module Fmt = CCFormat

(** A directory of problems, inside a known set of problems
    which defines what patterns to search for, and how to read "expect" results.
*)
type t = {
  path: string;
  inside: Dir.t;
}

let pp out (self:t) =
  let open Misc.Pp in
  Fmt.fprintf out "(@[%a@ :in %a@])" pp_str self.path Dir.pp self.inside

let pattern self = self.inside.Dir.pattern
let expect self = self.inside.Dir.expect

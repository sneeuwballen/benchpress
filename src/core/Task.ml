
module Fmt = CCFormat

type name = string

(** Description of a given task *)
type t = {
  name: name; (* name of this task *)
  synopsis: string option;
  action: Action.t;
  defined_in: string option;
}

let pp_name out self = Fmt.string out self.name

let pp out (self:t) =
  let open Misc.Pp in
  let {name;synopsis;action;defined_in} = self in
  Fmt.fprintf out "(@[<v1>task%a%a%a%a@])"
    (pp_f "name" pp_str) name
    (pp_opt "synopsis" pp_str) synopsis
    (pp_f "action" @@ Action.pp) action
    (pp_opt "defined_in" pp_str) defined_in
let to_string = Fmt.to_string pp


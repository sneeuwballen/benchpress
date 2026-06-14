module Fmt = CCFormat

type name = string

type t = {
  name: name; (* name of this task *)
  synopsis: string option;
  action: Action.t;
}
(** Description of a given task *)

let pp_name out self = Fmt.string out self.name

let pp out (self : t) =
  let open Misc.Pp in
  let { name; synopsis; action } = self in
  pp_record "task" out
    [
      field "name" pp_str name;
      field_opt "synopsis" pp_str synopsis;
      field "action" Action.pp action;
    ]

let to_string = Fmt.to_string pp

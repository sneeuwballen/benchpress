type pos = Loc.pos = { line: int; col: int }

type node =
  | Null
  | Bool of bool
  | Float of float
  | String of string
  | A of value list
  | O of (string * value) list

and value = { pos: pos; node: node }

val to_json : value -> string
(** Serialize to JSON string. *)

val to_yaml : value -> Yaml.value
(** Convert to Yaml.value for use with Yaml.to_string. *)

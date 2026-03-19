(* This file is free software. See file "license" for more details. *)

(** Lightweight JSON type and printer *)

type t =
  [ `String of string
  | `List of t list
  | `Assoc of (string * t) list
  | `Int of int
  | `Float of float
  | `Null ]

let rec pp out (self : t) : unit =
  match self with
  | `String s -> CCFormat.fprintf out "%S" s
  | `List l ->
    CCFormat.fprintf out "[@[%a@]]" (CCFormat.list ~sep:(CCFormat.return ",@ ") pp) l
  | `Assoc l ->
    let pp_pair out (s, v) = CCFormat.fprintf out "@[<1>%S:@ %a@]" s pp v in
    CCFormat.fprintf out "{@[%a@]}" (CCFormat.list ~sep:(CCFormat.return ",@ ") pp_pair) l
  | `Int i -> CCFormat.int out i
  | `Float f -> CCFormat.float out f
  | `Null -> CCFormat.string out "null"

let to_string (self : t) : string = CCFormat.asprintf "%a" pp self

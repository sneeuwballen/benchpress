(* This file is free software. See file "license" for more details. *)

module Fmt = CCFormat

type t = Sat | Unsat | Unknown | Timeout | Error | Tag of string

let to_string = function
  | Sat -> "sat"
  | Unsat -> "unsat"
  | Unknown -> "unknown"
  | Timeout -> "timeout"
  | Error -> "error"
  | Tag s -> s

let of_string ~tags = function
  | "sat" -> Sat
  | "unsat" -> Unsat
  | "error" -> Error
  | "timeout" -> Timeout
  | "unknown" -> Unknown
  | s when List.mem s tags -> Tag s
  | s -> failwith ("unknown result: " ^ s)

let pp out s = Fmt.string out (to_string s)

let compare a b =
  match a, b with
  | Unsat, Unsat
  | Sat, Sat
  | (Unknown | Timeout), (Unknown | Timeout)
  | Error, Error ->
    `Same
  | Tag s1, Tag s2 when s1 = s2 ->
    `Same
    (*
  | Unknown, Timeout -> `LeftBetter
  | Timeout, Unknown -> `RightBetter
       *)
  | (Unknown | Timeout | Error), (Sat | Unsat | Tag _) -> `RightBetter
  | (Sat | Unsat | Tag _), (Unknown | Timeout | Error) -> `LeftBetter
  | Error, (Unknown | Timeout) -> `RightBetter
  | (Unknown | Timeout), Error -> `LeftBetter
  | Unsat, Tag _
  | Sat, Tag _
  | Tag _, Unsat
  | Tag _, Sat
  | Unsat, Sat
  | Sat, Unsat
  | Tag _, Tag _ ->
    `Mismatch

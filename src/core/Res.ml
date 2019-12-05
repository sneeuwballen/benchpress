(* This file is free software. See file "license" for more details. *)

module Fmt = CCFormat

type t =
  | Sat
  | Unsat
  | Unknown
  | Timeout
  | Error

let to_string = function
  | Sat -> "sat"
  | Unsat -> "unsat"
  | Unknown -> "unknown"
  | Timeout -> "timeout"
  | Error -> "error"

let of_string = function
  | "sat" -> Sat
  | "unsat" -> Unsat
  | "error" -> Error
  | "timeout" -> Timeout
  | "unknown" -> Unknown
  | s -> failwith ("unknown result: " ^ s)

module J = Misc.Json
let encode s = J.Encode.(string (to_string s))
let decode = J.Decode.Infix.(
    J.Decode.string >>= fun s ->
    (try (J.Decode.succeed (of_string s)) with _ -> J.Decode.fail "invalid res")
  )

let pp out s = Fmt.string out (to_string s)

let compare a b = match a, b with
  | Unsat, Unsat
  | Sat, Sat
  | (Unknown | Timeout), (Unknown | Timeout)
  | Error, Error -> `Same
    (*
  | Unknown, Timeout -> `LeftBetter
  | Timeout, Unknown -> `RightBetter
       *)
  | (Unknown | Timeout | Error), (Sat | Unsat) -> `RightBetter
  | (Sat | Unsat), (Unknown | Timeout | Error) -> `LeftBetter
  | Error, (Unknown | Timeout) -> `RightBetter
  | (Unknown | Timeout), Error -> `LeftBetter
  | Unsat, Sat
  | Sat, Unsat ->
    `Mismatch


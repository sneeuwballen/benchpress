open Common

type t = Valid | Invalid | Unknown of string

let pp out = function
  | Valid -> Fmt.string out "valid"
  | Invalid -> Fmt.string out "invalid"
  | Unknown s -> Fmt.fprintf out "(invalid %s)" s

let to_string = function
  | Valid -> "valid"
  | Invalid -> "invalid"
  | Unknown u -> "unknown:" ^ u

let of_string s =
  match s with
  | "valid" -> Valid
  | "invalid" -> Invalid
  | s when CCString.prefix ~pre:"unknown:" s ->
    (match CCString.chop_prefix ~pre:"unknown:" s with
    | Some x -> Unknown x
    | None -> assert false)
  | _ -> Error.failf "unknown proof-check-res %S" s

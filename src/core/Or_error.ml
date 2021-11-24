
open Common
module E = CCResult

type 'a t = ('a, Error.t) result

let return x = Ok x
let fail ?loc msg = Error (Error.make ?loc msg)
let failf ?loc fmt =
  Fmt.kasprintf (fail ?loc) fmt

let wrap ?loc msg = E.map_err (Error.wrap ?loc msg)
let wrapf ?loc fmt =
  Fmt.kasprintf (fun msg -> E.map_err (Error.wrap ?loc msg)) fmt

let map = E.map
let map_l = E.map_l
let fold_l = E.fold_l
let flatten_l = E.flatten_l
let map_err = E.map_err
let of_exn ?loc e = Error (Error.of_exn ?loc e)

module Infix = E.Infix
include Infix

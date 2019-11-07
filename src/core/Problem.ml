
(* This file is free software. See file "license" for more details. *)

module E = CCResult
module StrMap = Misc.Str_map

[@@@warning "-39"]
type t = {
  name: string;  (* filename *)
  expected: Res.t; (* result expected *)
} [@@deriving yojson,eq]
[@@@warning "+39"]

type problem = t
type problem_set = t list

let basename t = Filename.basename t.name

let same_name t1 t2 = t1.name = t2.name
let hash_name t = CCHash.string t.name
let compare_name t1 t2 = Pervasives.compare t1.name t2.name

let make name expected =
  { name; expected; }

let compare_res pb res = match pb.expected, res with
  | Res.Unsat, Res.Unsat
  | Res.Sat, Res.Sat
  | Res.Timeout, Res.Timeout
  | Res.Unknown, Res.Unknown
  | Res.Error, Res.Error -> `Same
  | Res.Timeout, Res.Unknown
  | Res.Unknown, Res.Timeout
  | (Res.Sat | Res.Unsat | Res.Error), (Res.Unknown | Res.Timeout) -> `Disappoint
  | (Res.Unsat | Res.Error), Res.Sat
  | (Res.Sat | Res.Error), Res.Unsat -> `Mismatch
  | (Res.Sat | Res.Unknown | Res.Timeout | Res.Unsat), Res.Error ->
    `Error
  | (Res.Unknown | Res.Timeout), (Res.Sat | Res.Unsat) ->
    `Improvement

let pp out p =
  Format.fprintf out "@[<h>%s (expect: %a)@]" p.name Res.print p.expected

let to_string = CCFormat.to_string pp

module Tbl = struct
  type t = problem StrMap.t

  let empty = StrMap.empty
  let add pb t = StrMap.add pb.name pb t
  let add_l = List.fold_right add
  let find_by_name n t = try Some (StrMap.find n t) with Not_found -> None
  let to_list t = StrMap.fold (fun _ pb acc->pb::acc) t []
end

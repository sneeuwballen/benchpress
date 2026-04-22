(* This file is free software. See file "license" for more details. *)

(** Pretty-printing utilities *)

module Fmt = CCFormat

let pp_list ?(sep = " ") f out l =
  let sep out () = Fmt.fprintf out "%s@," sep in
  Fmt.list ~sep f out l

let pp_l = pp_list
let pp_pair f g out (x, y) = Fmt.fprintf out "(@[%a@ %a@])" f x g y
let pp_f what f out x = Fmt.fprintf out "@ (@[%s@ %a@])" what f x

let pp_opt what f out = function
  | None -> ()
  | Some x -> Fmt.fprintf out "@ (@[%s@ %a@])" what f x

let pp_fl1 what f out = function
  | [] -> ()
  | l -> pp_f what (pp_l f) out l

let pp_l1 f out l =
  if l = [] then
    ()
  else
    Fmt.fprintf out "@,%a" (pp_l f) l

let pp_str out s = Sexp_loc.pp out (Sexp_loc.atom s)
let pp_regex out r = Fmt.fprintf out "%S" r

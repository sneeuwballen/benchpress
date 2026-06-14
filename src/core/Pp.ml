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

let pp_str out s = Fmt.fprintf out "%S" s
let pp_regex out r = Fmt.fprintf out "%S" r

(** {2 YAML record combinators} *)

type field =
  | Field : {
      name: string;
      pp: Format.formatter -> 'a -> unit;
      value: 'a;
      skip: bool;
    }
      -> field

let field name pp value = Field { name; pp; value; skip = false }

let field_opt name pp = function
  | None -> Field { name; pp = (fun _ _ -> ()); value = (); skip = true }
  | Some v -> Field { name; pp; value = v; skip = false }

let field_list name pp = function
  | [] -> Field { name; pp = (fun _ _ -> ()); value = (); skip = true }
  | l ->
    let pp_list out l =
      Format.fprintf out "@,";
      let first = ref true in
      List.iter
        (fun x ->
          if !first then
            first := false
          else
            Format.fprintf out "@,";
          Format.fprintf out "  - %a" pp x)
        l
    in
    Field { name; pp = pp_list; value = l; skip = false }

(** Print a YAML record. Each field is indented 2 spaces relative to [name],
    and nested records are indented further via Format's box model.
    The output looks like:
    {v
      name:
        field1: val1
        field2: val2
    } *)
let pp_record name out fields =
  (* Use a <v2> box so @, breaks indent by 2 from the box start *)
  Format.fprintf out "@[<v2>%s:" name;
  List.iter
    (fun (Field { name; pp; value; skip }) ->
      if not skip then Format.fprintf out "@,%s: %a" name pp value)
    fields;
  Format.fprintf out "@]"

(** {2 YAML-style helpers (kept for backward compatibility)} *)

let pp_yaml_field name f out x = Fmt.fprintf out "@\n  %s: %a" name f x

let pp_yaml_field_opt name f out = function
  | None -> ()
  | Some x -> Fmt.fprintf out "@\n  %s: %a" name f x

let pp_yaml_list f out l =
  if l = [] then
    ()
  else (
    let pp_item out x = Fmt.fprintf out "@\n    - %a" f x in
    Format.pp_print_list ~pp_sep:(fun _out () -> ()) pp_item out l
  )

let pp_yaml_list_field name f out l =
  if l = [] then
    ()
  else
    Fmt.fprintf out "@\n  %s:%a" name (pp_yaml_list f) l

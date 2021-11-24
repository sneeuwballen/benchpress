(* This file is free software. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

open Misc

type result = Prover.name Run_result.t
type 'a or_error = 'a Or_error.t

module Log = (val Logs.src_log (Logs.Src.create "benchpress.test"))

(** {2 URL providers} *)

type string_linker = string -> string
type prover_string_linker = Prover.name -> string_linker
type path_linker = Problem.path -> PrintBox.t
type prover_path_linker = Prover.name -> Problem.path -> PrintBox.t
type prover_path_res_linker = Prover.name -> Problem.path -> res:string -> PrintBox.t

let default_linker path = PB.text path
let default_pp_linker _ path = default_linker path
let default_ppr_linker _ _ ~res = default_linker res


(** {2 DB helpers} *)

(** Cleanup a wildcard query for sqlite filtering *)
let clean_s wildcard s =
  let s = String.trim s in
  if s="" then "%" else if wildcard then "%"^s^"%" else s

(** Printers} *)


let assoc_or def x l =
  try List.assoc x l
  with Not_found -> def

let pp_list_ p =
  Fmt.within "(" ")"
    (Fmt.hovbox
       (Fmt.(list ~sep:(return "@ ") p)))

let pp_hvlist_ p =
  Fmt.within "(" ")"
    (Fmt.hvbox
       (Fmt.(list ~sep:(return "@ ") p)))

let time_of_res e = e.Run_result.raw.rtime

let pb_v_record ?bars l =
  PB.grid_l ?bars
    (CCList.map (fun (field,value) -> [PB.text field; value]) l)

let pb_int_color c n =
  let open PB in
  if n=0 then int n
  else text_with_style (Style.set_bold true c) (string_of_int n)

(* list provers from the main table *)
let list_provers db : string list or_error =
  Db.exec_no_params db
    {| select distinct prover from prover_res ; |}
    ~ty:Db.Ty.(p1 text, id) ~f:Db.Cursor.to_list_rev
  |> Misc.db_err_with ~ctx:"listing provers"



(** JSON helpers shared across the benchpress_mcp library. *)

let assoc_string key (j : Yojson.Basic.t) : string option =
  match j with
  | `Assoc fields ->
    (match List.assoc_opt key fields with
    | Some (`String s) -> Some s
    | _ -> None)
  | _ -> None

let assoc_field key (j : Yojson.Basic.t) : Yojson.Basic.t option =
  match j with
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let assoc_int key (j : Yojson.Basic.t) : int option =
  match j with
  | `Assoc fields ->
    (match List.assoc_opt key fields with
    | Some (`Int i) -> Some i
    | _ -> None)
  | _ -> None

let assoc_string_or key ~default j =
  match assoc_string key j with
  | Some s -> s
  | None -> default

let assoc_int_or key ~default j =
  match assoc_int key j with
  | Some i -> i
  | None -> default

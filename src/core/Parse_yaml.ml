let scalar_node (s : string) : Config_value.node =
  match String.lowercase_ascii s with
  | "null" | "~" | "" -> Null
  | "true" -> Bool true
  | "false" -> Bool false
  | _ ->
    (match float_of_string_opt s with
    | Some f -> Float f
    | None -> String s)

let parse (content : string) ~(file : string) :
    (Config_value.value, string) result =
  let module S = Yaml.Stream in
  let p =
    match S.parser content with
    | Ok p -> p
    | Error (`Msg m) ->
      failwith (Printf.sprintf "%s: YAML parse error: %s" file m)
  in
  let next () : S.Event.t * S.Event.pos =
    match S.do_parse p with
    | Ok x -> x
    | Error (`Msg m) ->
      failwith (Printf.sprintf "%s: YAML parse error: %s" file m)
  in
  let pos_of_mark (m : S.Mark.t) : Config_value.pos =
    let S.Mark.{ line; column; _ } = m in
    { Config_value.line; col = column }
  in
  let _ = next () in
  let _ = next () in
  let rec node () : Config_value.value =
    let ev, { S.Event.start_mark; _ } = next () in
    node_of_event ev start_mark
  and node_of_event ev (start_mark : S.Mark.t) : Config_value.value =
    match ev with
    | S.Event.Scalar { value; _ } ->
      let pos = pos_of_mark start_mark in
      { Config_value.pos; node = scalar_node value }
    | S.Event.Sequence_start _ ->
      let items = seq_items [] in
      let pos = pos_of_mark start_mark in
      { Config_value.pos; node = A items }
    | S.Event.Mapping_start _ ->
      let pairs = map_pairs [] in
      let pos = pos_of_mark start_mark in
      { Config_value.pos; node = O pairs }
    | _ ->
      let S.Mark.{ line; column; _ } = start_mark in
      failwith
        (Printf.sprintf "%s: unexpected YAML event at %d:%d" file line column)
  and seq_items acc =
    let ev, { S.Event.start_mark; _ } = next () in
    match ev with
    | S.Event.Sequence_end -> List.rev acc
    | _ ->
      let v = node_of_event ev start_mark in
      seq_items (v :: acc)
  and map_pairs acc =
    let ev, { S.Event.start_mark; _ } = next () in
    match ev with
    | S.Event.Mapping_end -> List.rev acc
    | S.Event.Scalar { value = key; _ } ->
      let v = node () in
      map_pairs ((key, v) :: acc)
    | _ ->
      let S.Mark.{ line; column; _ } = start_mark in
      failwith
        (Printf.sprintf "%s: expected mapping key at %d:%d" file line column)
  in
  try
    let v = node () in
    (try ignore (next ()) with _ -> ());
    (try ignore (next ()) with _ -> ());
    Ok v
  with Failure msg -> Result.error msg

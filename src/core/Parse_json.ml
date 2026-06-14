let parse (content : string) ~(file : string) :
    (Config_value.value, string) result =
  let d = Jsonm.decoder (`String content) in
  let pos_of_range ((l1, c1), (_, _)) =
    (* jsonm lines are 1-based, cols are 0-based *)
    { Config_value.line = l1; col = c1 }
  in
  let node_of_atom (pos : Config_value.pos) = function
    | `Null -> { Config_value.pos; node = Null }
    | `Bool b -> { Config_value.pos; node = Bool b }
    | `Float f -> { Config_value.pos; node = Float f }
    | `String s -> { Config_value.pos; node = String s }
    | _ -> assert false
  in
  let rec value v k =
    match v with
    | `Os -> obj [] k
    | `As -> arr [] k
    | (`Null | `Bool _ | `Float _ | `String _) as lex ->
      let pos = pos_of_range (Jsonm.decoded_range d) in
      k (node_of_atom pos lex)
    | _ -> assert false
  and arr vs k =
    match Jsonm.decode d with
    | `Lexeme `Ae ->
      let pos = pos_of_range (Jsonm.decoded_range d) in
      k { Config_value.pos; node = A (List.rev vs) }
    | `Lexeme v -> value v (fun v -> arr (v :: vs) k)
    | `Error e ->
      let (l1, c1), _ = Jsonm.decoded_range d in
      Error
        (Printf.sprintf "%s:%d:%d: JSON parse error: %s" file l1 c1
           (Format.asprintf "%a" Jsonm.pp_error e))
    | `End | `Await -> assert false
  and obj ms k =
    match Jsonm.decode d with
    | `Lexeme `Oe ->
      let pos = pos_of_range (Jsonm.decoded_range d) in
      k { Config_value.pos; node = O (List.rev ms) }
    | `Lexeme (`Name n) ->
      (match Jsonm.decode d with
      | `Lexeme v -> value v (fun v -> obj ((n, v) :: ms) k)
      | `Error e ->
        let (l1, c1), _ = Jsonm.decoded_range d in
        Error
          (Printf.sprintf "%s:%d:%d: JSON parse error in member '%s': %s" file
             l1 c1 n
             (Format.asprintf "%a" Jsonm.pp_error e))
      | `End | `Await -> assert false)
    | `Error e ->
      let (l1, c1), _ = Jsonm.decoded_range d in
      Error
        (Printf.sprintf "%s:%d:%d: JSON parse error: %s" file l1 c1
           (Format.asprintf "%a" Jsonm.pp_error e))
    | _ -> assert false
  in
  match Jsonm.decode d with
  | `Lexeme v -> value v (fun v -> Ok v)
  | `Error e ->
    let (l1, c1), _ = Jsonm.decoded_range d in
    Error
      (Printf.sprintf "%s:%d:%d: JSON parse error: %s" file l1 c1
         (Format.asprintf "%a" Jsonm.pp_error e))
  | `End | `Await -> Error "empty JSON input"

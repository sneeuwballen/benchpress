type pos = Loc.pos = { line: int; col: int }

type node =
  | Null
  | Bool of bool
  | Float of float
  | String of string
  | A of value list
  | O of (string * value) list

and value = { pos: pos; node: node }

(* JSON serialization *)

let check_ok = function
  | `Ok -> ()
  | `Partial -> failwith "Jsonm.encode: unexpected Partial"

let rec encode_json (e : Jsonm.encoder) (v : value) =
  match v.node with
  | Null -> check_ok (Jsonm.encode e (`Lexeme `Null))
  | Bool b -> check_ok (Jsonm.encode e (`Lexeme (`Bool b)))
  | Float f -> check_ok (Jsonm.encode e (`Lexeme (`Float f)))
  | String s -> check_ok (Jsonm.encode e (`Lexeme (`String s)))
  | A items ->
    check_ok (Jsonm.encode e (`Lexeme `As));
    List.iter (encode_json e) items;
    check_ok (Jsonm.encode e (`Lexeme `Ae))
  | O fields ->
    check_ok (Jsonm.encode e (`Lexeme `Os));
    List.iter
      (fun (k, v) ->
        check_ok (Jsonm.encode e (`Lexeme (`Name k)));
        encode_json e v)
      fields;
    check_ok (Jsonm.encode e (`Lexeme `Oe))

let to_json (v : value) : string =
  let buf = Buffer.create 256 in
  let e = Jsonm.encoder ~minify:false (`Buffer buf) in
  encode_json e v;
  check_ok (Jsonm.encode e `End);
  Buffer.contents buf

(* YAML conversion *)

let rec to_yaml (v : value) : Yaml.value =
  match v.node with
  | Null -> `Null
  | Bool b -> `Bool b
  | Float f -> `Float f
  | String s -> `String s
  | A items -> `A (List.map to_yaml items)
  | O fields -> `O (List.map (fun (k, v) -> k, to_yaml v) fields)

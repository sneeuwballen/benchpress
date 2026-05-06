(* Thin converter from benchpress sexp config to Lua.
   Uses only CCSexp (from containers), not the heavy Stanza machinery. *)

type sexp = [ `Atom of string | `List of sexp list ]

let lua_str s = Printf.sprintf "%S" s

(* Convert a sexp pair like (key value) to Lua table field [key = "value"].
   Returns None if we don't know how to handle it. *)
let pair_to_lua (key : string) (value : sexp) : string option =
  match key, value with
  (* skip complex fields with no direct Lua equivalent *)
  | "version", `List _ ->
    Some (Printf.sprintf "  -- version = ..., -- TODO: migrate manually")
  | "stack", _ ->
    Some (Printf.sprintf "  -- stack = ..., -- TODO: migrate manually")
  | "ulimits", _ ->
    Some (Printf.sprintf "  -- ulimits = ..., -- TODO: migrate manually")
  | "custom", _ ->
    Some (Printf.sprintf "  -- custom = ..., -- TODO: migrate manually")
  | "dir_files", _ ->
    Some (Printf.sprintf "  -- dir_files = ..., -- TODO: migrate manually")
  | "expect", `List _ ->
    Some
      (Printf.sprintf
         "  -- expect = ..., -- TODO: migrate manually (complex expect)")
  | "version", `Atom v ->
    (* version: "cmd:..." or plain string *)
    if String.length v > 4 && String.sub v 0 4 = "cmd:" then
      Some
        (Printf.sprintf
           "  -- version = %s, -- TODO: version cmd not supported in Lua"
           (lua_str v))
    else
      Some
        (Printf.sprintf "  -- version = %s, -- TODO: migrate manually"
           (lua_str v))
  | _, `Atom v -> Some (Printf.sprintf "  %s = %s," key (lua_str v))
  | _, `List items ->
    (* try to interpret as a list of atoms *)
    let atoms =
      List.filter_map
        (function
          | `Atom a -> Some a
          | _ -> None)
        items
    in
    if List.length atoms = List.length items && items <> [] then
      Some
        (Printf.sprintf "  %s = { %s }," key
           (String.concat ", " (List.map lua_str atoms)))
    else
      None

(* Known top-level constructors and their Lua counterparts *)
let top_level_name = function
  | "prover" -> Some "benchpress.prover"
  | "dir" -> Some "benchpress.dir"
  | "task" -> Some "benchpress.task"
  | "proof-checker" | "proof_checker" -> Some "benchpress.proof_checker"
  | _ -> None

let convert_fields (fields : sexp list) : string =
  let buf = Buffer.create 64 in
  List.iter
    (fun field ->
      match field with
      | `List [ `Atom key; value ] ->
        (match pair_to_lua key value with
        | Some s ->
          Buffer.add_string buf s;
          Buffer.add_char buf '\n'
        | None ->
          Buffer.add_string buf
            (Printf.sprintf "  -- %s = ..., -- TODO: migrate manually\n" key))
      | `Atom _ ->
        (* standalone atoms — skip silently *)
        ()
      | _ ->
        Buffer.add_string buf "  -- (complex field, TODO: migrate manually)\n")
    fields;
  Buffer.contents buf

let sexp_to_lua (buf : Buffer.t) (s : sexp) : unit =
  match s with
  | `List (`Atom name :: fields) ->
    (match top_level_name name with
    | Some lua_name ->
      Buffer.add_string buf lua_name;
      Buffer.add_string buf " {\n";
      Buffer.add_string buf (convert_fields fields);
      Buffer.add_string buf "}\n\n"
    | None ->
      Buffer.add_string buf
        (Printf.sprintf
           "-- unknown top-level form: %s (TODO: migrate manually)\n\n" name))
  | `Atom a ->
    Buffer.add_string buf (Printf.sprintf "-- standalone atom: %s\n\n" a)
  | `List _ ->
    Buffer.add_string buf
      "-- unknown top-level form (TODO: migrate manually)\n\n"

let sexp_str_to_lua_str (input : string) ~(filename : string) :
    (string, string) result =
  match CCSexp.parse_string_list input with
  | Error e -> Error (Printf.sprintf "%s: parse error: %s" filename e)
  | Ok sexps ->
    let buf = Buffer.create 256 in
    Buffer.add_string buf
      (Printf.sprintf "-- Converted from %s by benchpress convert-config\n\n"
         (Filename.basename filename));
    List.iter (sexp_to_lua buf) sexps;
    Ok (Buffer.contents buf)

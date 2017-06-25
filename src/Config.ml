(* This file is free software. See file "license" for more details. *)

(** {1 Config File} *)

module E = CCResult

type 'a or_error = ('a, string) E.t
type file = string

let error msg = Error msg
let errorf msg = CCFormat.ksprintf ~f:error msg

type table = TomlTypes.table
type t = {
  configs: (file * table) list;
}

let empty = {configs=[]}

let top_tbl ?(file="") tbl = {configs=[file, tbl]}

let create () = top_tbl ?file:None TomlTypes.Table.empty

let merge (c1:t)(c2:t) : t = {configs=List.append c1.configs c2.configs}


let rec merge_l = function
  | [] -> empty
  | [c] -> c
  | c :: tail -> merge c (merge_l tail)

(* obtain $HOME *)
let get_home () =
  let ic = Unix.open_process_in "echo $HOME" in
  let home = input_line ic in
  close_in ic;
  home

let interpolate_home s =
  let h = lazy (get_home ()) in
  let buf = Buffer.create (String.length s) in
  Buffer.add_substitute buf
    (function
      | "HOME" | "home" -> Lazy.force h
      | s -> failwith ("couldn't find variable: " ^ s))
    s;
  Buffer.contents buf

(* try to parse a config file *)
let parse_file file =
  begin match Toml.Parser.from_filename file with
    | `Ok tbl -> Ok (top_tbl ~file tbl)
    | `Error (msg, {Toml.Parser. source; line; column; _ }) ->
      errorf "config.parse_file `%s`: %s line %d column %d: %s"
        file source line column msg
    | exception e ->
      errorf "config.parse_file `%s`: %s" file (Printexc.to_string e);
  end

let parse_or_empty file = match parse_file file with
  | Error msg ->
    Format.eprintf "%s@." msg;
    empty
  | Ok x -> x

(* local exception *)
exception Error_files of string * string

let parse_files l: t or_error =
  let open E.Infix in
  E.map_l parse_file l >|= merge_l

(* Exceptions for getters *)

type error = error_msg * string list
and error_msg =
  | Try of error list
  | Field_not_found of string list
  | Wrong_type of string list * string
  | User_err of string

module Fmt = CCFormat

let pp_path = Fmt.(hbox @@ list ~sep:(return ".") string)
let string_of_path = Fmt.to_string pp_path

let rec pp_error out (e,l) =
  Fmt.fprintf out "@[<hv>%a%a@]" pp_error_msg e Fmt.(list string) l
and pp_error_msg out = function
  | Try [e] -> pp_error out e
  | Try l ->
    Fmt.fprintf out "(@[<hv>%a@])"
      Fmt.(list ~sep:(return "@ | ") pp_error) l
  | Field_not_found path ->
    Fmt.fprintf out "field not found: %a" pp_path path
  | Wrong_type (path, msg) ->
    Fmt.fprintf out "field %a has wrong type: %s" pp_path path msg
  | User_err s -> Fmt.string out s

let string_of_error = Fmt.to_string pp_error

type 'a getter = {
  path: string list;
  call: path:string list -> table -> ('a,error) result;
}

type 'a field_getter = ?default:'a -> string -> 'a getter

let return x : _ getter =
  {path=[]; call=fun ~path:_ _ -> Ok x}

let pure = return

let fail e : _ getter =
  { path=[]; call=fun ~path:_ _ -> Error (User_err e,[])}

let pure_or_error x =
  begin match x with
    | Error e -> fail e
    | Ok x -> pure x
  end

let add_ctx msg g = {
  path=g.path;
  call=fun ~path x ->
    begin match g.call ~path x with
      | Ok x -> Ok x
      | Error (e,l) -> Error (e, (("context:" ^ msg) :: l))
    end;
}

let add_ctxf msg =
  Fmt.ksprintf ~f:(fun msg -> add_ctx msg) msg

let top : table getter =
  {path=[]; call=fun ~path:_ tbl -> Ok tbl}

(* one-step getter *)
let field_ ?default name (f:TomlTypes.value -> 'a option) : 'a getter =
  {path=[name];
   call=fun ~path tbl ->
     let path = path @ [name] in
     begin match TomlTypes.Table.find (Toml.key name) tbl |> f with
       | None ->
         begin match default with
           | Some v -> Ok v
           | None -> Error (Field_not_found path,[])
         end
       | Some res -> Ok res
       | exception Not_found -> Error (Field_not_found path,[])
       | exception (TomlTypes.Table.Key.Bad_key _) -> Error (Field_not_found path,[])
     end;
  }

let bool ?default name =
  field_ ?default name
    (function
      | TomlTypes.TBool b -> Some b
      | _ -> None)

let map f g: _ getter =
  {path=g.path;
   call=fun ~path c ->
     begin match g.call ~path c with
       | Error _ as e -> e
       | Ok x -> Ok (f x)
     end;
  }

let flat_map f g: _ getter =
  {path=g.path;
   call=fun ~path c ->
     begin match g.call ~path c with
       | Error _ as e -> e
       | Ok x -> (f x).call ~path c
     end;
  }


let (>|=) g f = map f g
let (>>=) g f = flat_map f g

let int ?default name =
  field_ ?default name
    (function
      | TomlTypes.TInt x -> Some x
      | _ -> None)

let string ?default name =
  field_ ?default name
    (function
      | TomlTypes.TString x -> Some x
      | _ -> None)

let float ?default name =
  field_ ?default name
    (function
      | TomlTypes.TFloat x -> Some x
      | _ -> None)

let string_list ?default name =
  field_ ?default name
    (function
      | TomlTypes.TArray (TomlTypes.NodeString l) -> Some l
      | _ -> None)

let table ?default name =
  field_ ?default name
    (function
      | TomlTypes.TTable t -> Some t
      | _ -> None)

let none = return None
let some g: _ option getter = g >|= fun x -> Some x

let (|>>) (k:table getter) (g:'a getter) : 'a getter =
  {path=k.path @ g.path;
   call=fun ~path tbl ->
    begin match k.call ~path tbl with
      | Error _ as e -> e
      | Ok tbl -> g.call ~path:(path @ k.path) tbl
    end
  }

let rec table_l l = match l with
  | [] -> top
  | [p] -> table p
  | p :: p_tail -> table p |>> (table_l p_tail)

let try_ (g_l:'a getter list) : 'a getter =
  let rec aux errors g_l ~path tbl = match g_l with
    | [] -> Error (Try errors,[])
    | g1 :: g_tail ->
      begin match g1.call ~path tbl with
        | Ok x -> Ok x (* success *)
        | Error (Try es,_) -> aux (es @ errors) g_tail ~path tbl (* flatten *)
        | Error e -> aux (e :: errors) g_tail ~path tbl
      end
  in
  {path=[];
   call=aux [] g_l;
  }

let try_tables tbls g : _ getter =
  try_ (List.map (fun tbl -> tbl |>> g) tbls)

let (<|>) a b = try_ [a;b]

let lazy_ (g:'a lazy_t getter): 'a getter = g >|= Lazy.force

let map_l f l =
  let rec aux acc = function
    | [] -> return (List.rev acc)
    | x :: tail -> f x >>= fun x' -> aux (x' :: acc) tail
  in
  aux [] l

(* how to use a getter *)
let get (c:t) (g:'a getter) : 'a or_error =
  (* try tables one by one *)
  let rec aux errors l: _ or_error = match l with
    | [] -> Error (string_of_error (Try errors,[]))
    | (file,c) :: tail ->
      let path = match file with
        | "" -> []
        | _ -> [Printf.sprintf "file(`%s`)" file]
      in
      (* try [g] on [c], else fallback on [tail] *)
      begin match g.call ~path c with
        | CCResult.Ok x -> CCResult.Ok x
        | CCResult.Error e -> aux (e :: errors) tail
      end
  in
  aux [] c.configs

let get_or ~default c g = match get c g with
  | CCResult.Ok x -> x
  | CCResult.Error _ -> default

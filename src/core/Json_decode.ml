(* This file is free software. See file "license" for more details. *)

type value = Config_value.value
type path = Root | Field of string * path | Index of int * path

let path_to_string =
  let rec go buf = function
    | Root -> Buffer.add_string buf "$"
    | Field (name, parent) ->
      go buf parent;
      Buffer.add_char buf '.';
      Buffer.add_string buf name
    | Index (i, parent) ->
      go buf parent;
      Buffer.add_char buf '[';
      Buffer.add_string buf (string_of_int i);
      Buffer.add_char buf ']'
  in
  fun p ->
    let buf = Buffer.create 32 in
    go buf p;
    Buffer.contents buf

type err = { msg: string; path: path; value: value; ctx_of: err option }

exception Decode_error of err

type +'a t = { run: path -> value -> ('a, err) result }

let return x = { run = (fun _ _ -> Ok x) }

let fail msg =
  { run = (fun path value -> Error { msg; path; value; ctx_of = None }) }

let failf fmt = Printf.ksprintf fail fmt

let ( let+ ) d f =
  {
    run =
      (fun path value ->
        match d.run path value with
        | Ok x -> Ok (f x)
        | Error _ as e -> e);
  }

let ( let* ) d f =
  {
    run =
      (fun path value ->
        match d.run path value with
        | Ok x -> (f x).run path value
        | Error _ as e -> e);
  }

let ( >|= ) x f = ( let+ ) x f
let ( >>= ) x f = ( let* ) x f

let fix (f : 'a t -> 'a t) : 'a t =
  let r = ref None in
  let self =
    {
      run =
        (fun path value ->
          !r |> CCOption.get_exn_or "Json_decode.fix: not initialized"
          |> fun d -> d.run path value);
    }
  in
  let d = f self in
  r := Some d;
  d

let string : string t =
  {
    run =
      (fun path value ->
        match value.node with
        | String s -> Ok s
        | _ -> Error { msg = "expected a string"; path; value; ctx_of = None });
  }

let int : int t =
  {
    run =
      (fun path value ->
        match value.node with
        | Float f ->
          let i = int_of_float f in
          if float_of_int i = f then
            Ok i
          else
            Error { msg = "expected an integer"; path; value; ctx_of = None }
        | _ -> Error { msg = "expected an integer"; path; value; ctx_of = None });
  }

let bool : bool t =
  {
    run =
      (fun path value ->
        match value.node with
        | Bool b -> Ok b
        | _ -> Error { msg = "expected a boolean"; path; value; ctx_of = None });
  }

let option (d : 'a t) : 'a option t =
  {
    run =
      (fun path value ->
        match value.node with
        | Null -> Ok None
        | _ ->
          (match d.run path value with
          | Ok x -> Ok (Some x)
          | Error e -> Error e));
  }

let list (d : 'a t) : 'a list t =
  {
    run =
      (fun path value ->
        match value.node with
        | A items ->
          let exception E of err in
          (try
             Ok
               (List.mapi
                  (fun i item ->
                    match d.run (Index (i, path)) item with
                    | Ok x -> x
                    | Error e -> raise (E e))
                  items)
           with E e -> Error e)
        | _ -> Error { msg = "expected an array"; path; value; ctx_of = None });
  }

let field (name : string) (d : 'a t) : 'a t =
  {
    run =
      (fun path value ->
        match value.node with
        | O fields ->
          (match List.find_opt (fun (k, _) -> k = name) fields with
          | Some (_, v) -> d.run (Field (name, path)) v
          | None ->
            Error
              {
                msg = Printf.sprintf "missing required field '%s'" name;
                path;
                value;
                ctx_of = None;
              })
        | _ -> Error { msg = "expected an object"; path; value; ctx_of = None });
  }

let field_opt (name : string) (d : 'a t) : 'a option t =
  {
    run =
      (fun path value ->
        match value.node with
        | O fields ->
          (match List.find_opt (fun (k, _) -> k = name) fields with
          | None -> Ok None
          | Some (_, v) ->
            (match d.run (Field (name, path)) v with
            | Ok x -> Ok (Some x)
            | Error e -> Error e))
        | _ -> Error { msg = "expected an object"; path; value; ctx_of = None });
  }

let field_or (name : string) ~default (d : 'a t) : 'a t =
  let+ x = field_opt name d in
  match x with
  | None -> default
  | Some x -> x

let value : value t = { run = (fun _path value -> Ok value) }

let sub (d : 'a t) (v : value) : 'a t =
  { run = (fun path _value -> d.run path v) }

let run (d : 'a t) (value : value) : ('a, err) result = d.run Root value

let run_exn (d : 'a t) (value : value) : 'a =
  match run d value with
  | Ok x -> x
  | Error e -> raise (Decode_error e)

module Err = struct
  type t = err

  let rec to_string (e : t) : string =
    let base =
      Printf.sprintf "line %d: %s: %s" e.value.Config_value.pos.line
        (path_to_string e.path) e.msg
    in
    match e.ctx_of with
    | None -> base
    | Some inner -> base ^ "\n  caused by: " ^ to_string inner

  let pp fmt e = Fmt.string fmt (to_string e)
end

(* This file is free software. See file "license" for more details. *)

(** {1 Run Prover} *)

type version =
  | Tag of string
  | Git of string * string  (* branch & commit hash *)

type t = {
  (* Prover identification *)
  name : string;
  version : version;

  (* Pover execution *)
  binary: string;       (* name of the program itself *)
  binary_deps: (string list [@default []]); (* additional list of binaries this depends on *)
  cmd: string;          (* the command line to run.
                           possibly contains $binary, $file, $memory and $timeout *)

  (* Result analysis *)
  unsat   : string option;  (* regex for "unsat" *)
  sat     : string option;  (* regex for "sat" *)
  unknown : string option;  (* regex for "unknown" *)
  timeout : string option;  (* regex for "timeout" *)
  memory  : string option;  (* regex for "out of memory" *)
}

type t_ = t

let equal p1 p2 = p1.name = p2.name

let version_to_string = function
  | Tag s -> s
  | Git (b, c) -> Printf.sprintf "%s#%s" b c

let name p = p.name

let pp_name out p = Format.pp_print_string out p.name

exception Subst_not_found of string

(* Internal function, do NOT export ! *)
let mk_cmd
    ?(env=[||])
    ?(binary="")
    ?(timeout=0)
    ?(memory=0)
    ?(file="")
    cmd =
  let buf = Buffer.create 32 in
  let add_str s =
    Buffer.add_substitute buf
      (function
        | "memory" -> string_of_int memory
        | "timeout" | "time" -> string_of_int timeout
        | "file" -> file
        | "binary" -> binary
        | s -> raise (Subst_not_found s))
      s
  in
  add_str "ulimit -t \\$(( 1 + $time)) -v \\$(( 1200 * $memory )); ";
  Array.iter
    (fun (key,value) -> add_str (key ^ "=" ^ value ^ " "))
    env;
  add_str cmd;
  Buffer.contents buf

let make_command ?env prover ~timeout ~memory ~file =
  let binary = prover.binary in
  try mk_cmd ?env ~binary ~timeout ~memory ~file prover.cmd
  with Subst_not_found s ->
    failwith (Printf.sprintf
        "cannot make command for prover %s: cannot find field %s" prover.name s)

module Map_name = CCMap.Make(struct
    type t = t_
    let compare a b = String.compare a.name b.name
  end)

module As_key = struct
  type t = t_

  let compare p1 p2 =
    let c = String.compare p1.name p2.name in
    if c<>0 then c else Pervasives.compare p1.version p2.version
end

module Map = CCMap.Make(As_key)
module Set = CCSet.Make(As_key)
    
module J = Misc.Json

let encode_version v =
  let open J.Encode in
  match v with
  | Tag s -> list string ["tag"; s]
  | Git (br,commit) ->
    list string ["git"; br; commit]

let decode_version =
  let open J.Decode in
  string >>:: function
  | "tag" -> (list1 string >|= fun s -> Tag s)
  | "git" ->
    (list string >>= function
    | [br;commit] -> succeed (Git (br,commit))
    | _ -> fail "need 2 arguments")
  | _ -> fail "unknown constructor, expect tag/git"

let encode p =
  let open J.Encode in
  let {
    name;version;binary;binary_deps;cmd;
    sat; unsat; unknown; timeout; memory;
  } = p in
  obj [
    "name", string name;
    "version", encode_version version;
    "binary", string binary;
    "binary_deps", list string binary_deps;
    "cmd", string cmd;
    "unsat", option string unsat;
    "sat", option string sat;
    "unknown", option string unknown;
    "timeout", option string timeout;
    "memory", option string memory;
  ]

let decode =
  let open J.Decode in
  let f_opt_null f_name =
    field_opt f_name (nullable string) >|= CCOpt.flatten
  in
  field "name" string >>= fun name ->
  field "cmd" string >>= fun cmd ->
  field "binary" string >>= fun binary ->
  field "binary_deps" (list string) >>= fun binary_deps ->
  field "version" decode_version >>= fun version ->
  f_opt_null "unsat" >>= fun unsat ->
  f_opt_null "sat" >>= fun sat ->
  f_opt_null "unknown" >>= fun unknown ->
  f_opt_null "timeout" >>= fun timeout ->
  f_opt_null "memory" >>= fun memory ->
  succeed {
    name; cmd; binary; binary_deps; version;
    sat; unsat; unknown; timeout; memory }


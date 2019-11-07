
(* This file is free software. See file "license" for more details. *)

open Prover

module E = CCResult
module StrMap = Misc.Str_map

exception Subst_not_found of string

type 'a or_error = ('a, string) Result.result

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
  (* XXX: seems to ake zombie processes?
     add_str "ulimit -t \\$(( 1 + $time)) -v \\$(( 1000000 * $memory )); ";
  *)
  Array.iter
    (fun (key,value) -> add_str (key ^ "=" ^ value ^ " "))
    env;
  add_str cmd;
  Buffer.contents buf

(* obtain the current commit name *)
let get_cmd_out cmd =
  CCUnix.with_process_in cmd
    ~f:(fun ic -> CCIO.read_all ic |> String.trim)

let get_commit dir : string =
  get_cmd_out (
    Printf.sprintf "git -C %s rev-parse HEAD" dir)

let get_branch dir : string =
  get_cmd_out (
    Printf.sprintf "git -C %s branch | grep '*' | cut -d ' ' -f2" dir)

(* recover description of prover from config file *)
let build_from_config config name =
  let getter =
    let open Config in
    let prover_tbl = table name in
    prover_tbl |>> string "cmd" >|= String.trim >>= fun cmd ->
    begin
      (prover_tbl |>> string "binary")
      <|>
        ( let b, _ = CCString.Split.left_exn ~by:" " cmd in
          if b = "$binary" then
            fail ("please provide $binary value for prover " ^ name)
          else pure b)
    end >>= fun binary ->
    (prover_tbl |>> string_list "binary_deps" <|> pure [])
    >>= fun binary_deps ->
    begin
      (prover_tbl |>> string "version"
       >|= fun s ->
       begin match CCString.Split.left_exn ~by:":" s with
         | "git", dir ->
           Git (get_branch dir, get_commit dir)
         | "cmd", cmd ->
           begin try
               Tag (get_cmd_out @@ mk_cmd ~binary cmd)
             with Subst_not_found s ->
               Tag (Printf.sprintf "command `%s` failed: cannot find field %s" cmd s)
           end
         | _ -> Tag s
         | exception Not_found -> Tag s
       end)
      <|> pure (Tag "dev")
    end >>= fun version ->
    let get_stropt name =
      some (prover_tbl |>> string name) <|> pure None
    in
    get_stropt "unsat" >>= fun unsat ->
    get_stropt "sat" >>= fun sat ->
    get_stropt "timeout" >>= fun timeout ->
    get_stropt "unknown" >>= fun unknown ->
    get_stropt "memory" >>= fun memory ->
    pure {
      name; version; cmd; binary; binary_deps; unsat; sat;
      unknown; timeout; memory; }
  in
  Config.get config getter

let find_config config name =
  (* check that the prover is listed *)
  let provers =
    Config.(get_or ~default:[] config @@ string_list "provers")
  in
  if not (List.mem name provers) then (
    E.fail_fprintf "prover %s not listed in config" name
  ) else build_from_config config name

(* make a list of provers from the given config *)
let of_config config =
  let open E.Infix in
  begin
    Config.(get_or ~default:[] config @@ string_list "provers")
    |> List.map (build_from_config config)
    |> E.map_l CCFun.id
  end
  >|= fun l ->
  List.fold_left
    (fun map prover -> StrMap.add (Prover.name prover) prover map)
    StrMap.empty l


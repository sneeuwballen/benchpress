

open Common

type name = string
type t = {
  name: name;
  cmd: string; (* take $proof_file and $problem *)

  valid: string;
  invalid: string;
}

let pp out self =
  let open Misc.Pp in
  let {name; cmd; valid; invalid} = self in
  Fmt.fprintf out
    "(@[<hv1>proof-checker%a%a%a%a@])"
    (pp_f "name" pp_str) name
    (pp_f "cmd" pp_str) cmd
    (pp_f "valid" pp_regex) valid
    (pp_f "invalid" pp_regex) invalid

module Res = Proof_check_res

exception Subst_not_found of string

let interpolate_cmd_ ?(env=[||]) ~subst cmd =
  let buf = Buffer.create 32 in
  let add_str s = Buffer.add_substitute buf subst s in
  Array.iter
    (fun (key,value) -> add_str (key ^ "=" ^ value ^ " "))
    env;
  add_str cmd;
  Buffer.contents buf

let make_cmd ?env ~problem ~proof_file (self:t) : string =
  let subst = function
    | "problem" -> problem
    | "proof_file" -> proof_file
    | s -> raise (Subst_not_found s)
  in
  try interpolate_cmd_ ?env self.cmd ~subst
  with Subst_not_found s ->
    Error.raise @@ Error.makef
      "cannot make command for proof_checker %s:@ cannot find field %s"
      self.name s

let run ~problem ~proof_file (self:t) =
  let cmd = make_cmd ~problem ~proof_file self in
  Run_proc.run cmd

let analyze_res (self:t) (res:Run_proc_result.t) : Res.t =

  let find_ re =
    let re = Re.Perl.compile_pat ~opts:[`Multiline] re in
    Re.execp re res.stdout ||
    Re.execp re res.stderr
  in

  if find_ self.valid then Res.Valid
  else if find_ self.invalid then Res.Invalid
  else if res.errcode <> 0 then (
    Res.Unknown (spf "no match; errcode=%d" res.errcode)
  ) else (
    Res.Unknown "no match"
  )

let db_prepare (db:Db.t) : unit =
  Db.exec0 db {|
  create table if not exists
    proof_checker (
      name text not null unique,
      cmd blob not null,
      valid text not null,
      invalid text not null
    );
    |}
  |> Misc.unwrap_db (fun() -> "creating proof checker table")

let to_db db (self:t) : unit =
  Db.exec_no_cursor db
    {|insert into proof_checker values (?,?,?,?) on conflict do nothing;
      |}
    ~ty:Db.Ty.([text; text; blob; blob])
    self.name self.cmd self.valid self.invalid
  |> Misc.unwrap_db (fun() -> "proof_checker.to-db")

let db_names db : string list =
  Db.exec_no_params db
    {| select unique name from proof_checker ; |}
    ~f:Db.Cursor.to_list_rev ~ty:Db.Ty.([text], fun x->x)
  |> Misc.unwrap_db (fun() -> "obtaining list of proof checkers")

let of_db db (name:string) : t =
  Db.exec db
    {|select cmd, valid, invalid from proof_checker where name=? ; |}
    name ~f:Db.Cursor.next
    ~ty:Db.Ty.([text], [text;blob;blob],
               fun cmd valid invalid -> {name; cmd; valid; invalid})
  |> Misc.unwrap_db (fun() -> spf "parsing proof checker %s" name)
  |> Error.unwrap_opt (spf "expected a result when parsing proof checker %s" name)

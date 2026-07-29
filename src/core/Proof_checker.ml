open Common

type name = string

type t = {
  name: name;
  cmd: string;  (** take $proof_file and $problem *)
  valid: string;
  invalid: string;
  timeout: Limit.Time.t option;
      (** checker-specific timeout; falls back to prover limits then 600s *)
  memory: Limit.Memory.t option;
      (** checker-specific memory limit; falls back to prover limits then 1GB *)
}

let pp out self =
  let open Misc.Pp in
  let { name; cmd; valid; invalid; timeout; memory } = self in
  pp_record "proof-checker" out
    [
      field "name" pp_str name;
      field "cmd" pp_str cmd;
      field "valid" pp_regex valid;
      field "invalid" pp_regex invalid;
      field_opt "timeout" Limit.Time.pp timeout;
      field_opt "memory" Limit.Memory.pp memory;
    ]

module Res = Proof_check_res

exception Subst_not_found of string

let interpolate_cmd_ ?(env = [||]) ~subst cmd =
  let buf = Buffer.create 32 in
  let add_str s = Buffer.add_substitute buf subst s in
  Array.iter (fun (key, value) -> add_str (key ^ "=" ^ value ^ " ")) env;
  add_str cmd;
  Buffer.contents buf

let make_cmd ?env ~problem ~proof_file (self : t) : string =
  let subst = function
    | "file" -> problem
    | "proof_file" -> proof_file
    | s -> raise (Subst_not_found s)
  in
  try interpolate_cmd_ ?env self.cmd ~subst
  with Subst_not_found s ->
    Error.raise
    @@ Error.makef
         "cannot make command for proof_checker %s:@ cannot find field %s"
         self.name s

let run ?(limits = Limit.All.default) ~problem ~proof_file (self : t) =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "proof-checker.run" in
  Trace.add_data_to_span _sp
    [
      "checker", `String self.name;
      "problem", `String problem;
      "proof_file", `String proof_file;
    ];
  let hard_defaults =
    Limit.All.mk ~time:(Limit.Time.mk ~s:600 ())
      ~memory:(Limit.Memory.mk ~g:1 ()) ()
  in
  let limits = Limit.All.with_defaults ~defaults:hard_defaults limits in
  let limits =
    match self.timeout with
    | Some t -> { limits with time = Some t }
    | None -> limits
  in
  let limits =
    match self.memory with
    | Some m -> { limits with memory = Some m }
    | None -> limits
  in
  let limits =
    Limit.All.update_time (CCOpt.map Limit.Time.(add (mk ~s:1 ()))) limits
  in
  let cmd = make_cmd ~problem ~proof_file self in
  let ulimit = Ulimit.mk ~time:true ~memory:true ~stack:true in
  let cmd = Ulimit.prefix_cmd ~conf:ulimit ~limits ~cmd in
  Run_proc.run cmd

let analyze_res (self : t) (res : Run_proc_result.t) : Res.t option =
  let find_ re =
    let re = Re.Perl.compile_pat ~opts:[ `Multiline ] re in
    Re.execp re res.stdout || Re.execp re res.stderr
  in

  if find_ self.valid then
    Some Res.Valid
  else if find_ self.invalid then
    Some Res.Invalid
  else
    None

let db_prepare (db : Db.t) : unit =
  Db.exec0 db
    {|
  create table if not exists
    proof_checker (
      name text not null unique,
      cmd blob not null,
      valid text not null,
      invalid text not null,
      timeout integer,
      memory integer
    );
    |}
  |> Misc.unwrap_db ~db (fun () -> "creating proof checker table")

let to_db db (self : t) : unit =
  let timeout_int = Option.map (Limit.Time.as_int Seconds) self.timeout in
  let memory_int = Option.map (Limit.Memory.as_int Megabytes) self.memory in
  Db.exec_no_cursor db
    {|insert into proof_checker values (?,?,?,?,?,?) on conflict do nothing;
      |}
    ~ty:Db.Ty.[ text; text; blob; blob; nullable int; nullable int ]
    self.name self.cmd self.valid self.invalid timeout_int memory_int
  |> Misc.unwrap_db ~db (fun () -> "proof_checker.to-db")

let db_names db : string list =
  Db.exec_no_params db {| select unique name from proof_checker ; |}
    ~f:Db.Cursor.to_list_rev
    ~ty:Db.Ty.([ text ], fun x -> x)
  |> Misc.unwrap_db ~db (fun () -> "obtaining list of proof checkers")

let of_db db (name : string) : t =
  Db.exec db
    {|select cmd, valid, invalid, timeout, memory from proof_checker where name=? ; |}
    name ~f:Db.Cursor.next
    ~ty:
      Db.Ty.(
        ( [ text ],
          [ text; blob; blob; nullable int; nullable int ],
          fun cmd valid invalid timeout memory ->
            {
              name;
              cmd;
              valid;
              invalid;
              timeout = Option.map (fun s -> Limit.Time.mk ~s ()) timeout;
              memory = Option.map (fun m -> Limit.Memory.mk ~m ()) memory;
            } ))
  |> Misc.unwrap_db ~db (fun () -> spf "parsing proof checker %s" name)
  |> Error.unwrap_opt
       (spf "expected a result when parsing proof checker %s" name)

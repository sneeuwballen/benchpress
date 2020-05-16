(* This file is free software. See file "license" for more details. *)

(** {1 Run Prover} *)

module Fmt = CCFormat
module E = CCResult
module Db = Sqlite3_utils
type 'a or_error = ('a, string) E.t

let src_log = Logs.Src.create "prover"

type version =
  | Tag of string
  | Git of {
      branch: string;
      commit: string;  (* branch & commit hash *)
    }

type name = string

type t = {
  (* Prover identification *)
  name : name;
  version : version;

  (* Prover execution *)
  binary: string; (* name of the program itself *)
  binary_deps: (string list [@default []]); (* additional list of binaries this depends on *)
  cmd: string;
  (* the command line to run. Possibly contains $binary, $file, $memory and $timeout *)

  (* Result analysis *)
  unsat   : string option;  (* regex for "unsat" *)
  sat     : string option;  (* regex for "sat" *)
  unknown : string option;  (* regex for "unknown" *)
  timeout : string option;  (* regex for "timeout" *)
  memory  : string option;  (* regex for "out of memory" *)
  custom  : (string * string) list; (* custom tags *)
  defined_in: string option;
}

type t_ = t

let equal p1 p2 = p1.name = p2.name

let name p = p.name

let compare_name = String.compare
let pp_name out p = Fmt.string out p.name
let compare_by_name p1 p2 = compare_name p1.name p2.name

module Version = struct
  type t = version

  let to_string_short = function
    | Tag s -> s
    | Git {branch=b; commit=c} -> Printf.sprintf "%s#%s" b c

  let pp out =
    let open Misc.Pp in
    function
    | Tag s -> Fmt.fprintf out "(tag %a)" pp_str s
    | Git {branch=b; commit=c} ->
      Fmt.fprintf out "(@[git@ branch=%a@ commit=%a@])" pp_str b pp_str c

  let to_sexp = function
    | Tag s -> Sexp_loc.atom s
    | Git {branch; commit} ->
      let open Sexp_loc in
      of_list [
        atom "git";
        of_list [atom "branch"; atom branch];
        of_list [atom "commit"; atom commit];
      ]

  let sexp_decode =
    let open Sexp_loc.D in
    one_of [
      ("atom", string >|= fun s -> Tag s);
      ("list", string >>:: function
        | "git" ->
          field "branch" string >>= fun branch ->
          field "commit" string >>= fun commit ->
          succeed (Git{branch;commit})
        | _ -> fail "constructor should be 'git'")
    ]

  let ser_sexp v = Sexp_loc.to_string @@ to_sexp v
  let deser_sexp s =
    Sexp_loc.D.decode_string sexp_decode s
    |> E.map_err Sexp_loc.D.string_of_error
end

let pp out self =
  let open Misc.Pp in
  let {name; version; cmd; unsat; sat; timeout; unknown; memory;
       binary; custom; binary_deps=_; defined_in} = self in
  Fmt.fprintf out
    "(@[<hv1>prover%a%a%a%a%a%a%a%a%a%a%a@])"
    (pp_f "name" pp_str) name
    (pp_f "version" Version.pp) version
    (pp_f "cmd" pp_str) cmd
    (pp_f "binary" pp_str) binary
    (pp_opt "sat" pp_regex) sat
    (pp_opt "unsat" pp_regex) unsat
    (pp_opt "memory" pp_regex) memory
    (pp_opt "timeout" pp_regex) timeout
    (pp_opt "unknown" pp_regex) unknown
    (pp_opt "defined_in" pp_str) defined_in
    (pp_l1 (pp_pair pp_str pp_regex)) custom

exception Subst_not_found of string

let interpolate_cmd
    ?(env=[||])
    ?(binary="")
    ?(timeout=1)
    ?(memory=5_000_000)
    ?(file="")
    ?(f=fun _->None)
    cmd =
  let buf = Buffer.create 32 in
  let add_str s =
    Buffer.add_substitute buf
      (function
        | "memory" -> string_of_int memory
        | "timeout" | "time" -> string_of_int timeout
        | "file" -> file
        | "binary" -> binary
        | s ->
          match f s with
          | Some u -> u
          | None -> raise (Subst_not_found s))
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
  try interpolate_cmd ?env ~binary ~timeout ~memory ~file prover.cmd
  with Subst_not_found s ->
    failwith (Printf.sprintf
                "cannot make command for prover %s: cannot find field %s" prover.name s)

module Map_name = CCMap.Make(struct
    type t = t_
    let compare = compare_by_name
  end)

module As_key = struct
  type t = t_

  let compare p1 p2 =
    let c = String.compare p1.name p2.name in
    if c<>0 then c else CCOrd.compare p1.version p2.version
end

module Map = CCMap.Make(As_key)
module Set = CCSet.Make(As_key)

let run_proc cmd =
  let start = Unix.gettimeofday () in
  (* call process and block *)
  let p = try CCUnix.call_full "%s" cmd
    with e ->
      object
        method stdout=""
        method stderr="<process died: " ^ Printexc.to_string e
        method errcode=1
        method status = Unix.WEXITED 1
      end
  in
  let errcode = p#errcode in
  Logs.debug ~src:src_log
    (fun k->k "(@[prover.run.done errcode: %d@ cmd %a@]" errcode Misc.Pp.pp_str cmd);
  (* Compute time used by the prover *)
  let rtime = Unix.gettimeofday () -. start in
  let utime = 0. in
  let stime = 0. in
  let stdout = p#stdout in
  let stderr = p#stderr in
  Logs.debug ~src:src_log
    (fun k->k "stdout:\n%s\nstderr:\n%s" stdout stderr);
  { Proc_run_result. stdout; stderr; errcode; rtime; utime; stime; }

let run ?env ~timeout ~memory ~file (self:t) : Proc_run_result.t =
  Logs.debug ~src:src_log
    (fun k->k "(@[Prover.run %s timeout: %d, memory: %d@])" self.name timeout memory);
  (* limit time and memory ('v' is virtual memory, needed because 'm' ignored on linux) *)
  let memory' = memory * 1000 in
  let memory_v = memory * 1000 * 8 in (* give 8 times more virtual mem, for JVM *)
  let prefix =
    Printf.sprintf "ulimit -t %d -m %d -Sv %d; " timeout memory' memory_v
  in
  let cmd = make_command ?env self ~timeout ~memory ~file in
  let cmd = prefix ^ cmd in
  run_proc cmd

let analyze_p_opt (self:t) (r:Proc_run_result.t) : Res.t option =
  (* find if [re: re option] is present in [stdout] *)
  let find_ re =
    let re = Re.Perl.compile_pat ~opts:[`Multiline] re in
    Re.execp re r.stdout ||
    Re.execp re r.stderr
  in
  let find_opt_ re = match re with
    | None -> false
    | Some re -> find_ re
  in
  if find_opt_ self.sat then Some Res.Sat
  else if find_opt_ self.unsat then Some Res.Unsat
  else if find_opt_ self.timeout then Some Res.Timeout
  else if find_opt_ self.unknown then Some Res.Unknown
  else (
    (* look for custom tags *)
    CCList.find_map
      (fun (tag,re) -> if find_ re then Some (Res.Tag tag) else None)
      self.custom
  )

let db_prepare (db:Db.t) : unit or_error =
  Db.exec0 db {|
  create table if not exists
    prover (
      name text not null unique,
      version text not null,
      binary blob not null,
      unsat text not null,
      sat text not null,
      unknown text not null,
      timeout text not null,
      memory text not null
    );

  create table if not exists
    custom_tags (
      prover_name text not null,
      tag text not null,
      regex text not null
    );
  |}
  |> Misc.db_err ~ctx:"creating prover table"

let to_db db (self:t) : unit or_error =
  let str_or = CCOpt.get_or ~default:"" in
  Misc.err_with (fun scope ->
    Db.exec_no_cursor db
      {|insert into prover values (?,?,?,?,?,?,?,?) on conflict do nothing;
      |}
      ~ty:Db.Ty.(p3 text text blob @>> text @> p4 text text text text)
      self.name
      (Version.ser_sexp self.version)
      self.binary
      (self.unsat |> str_or)
      (self.sat |> str_or)
      (self.unknown |> str_or)
      (self.timeout |> str_or)
      (self.memory |> str_or)
      |> Misc.db_err ~ctx:"prover.to-db" |> scope.unwrap;
    if self.custom <> [] then (
      List.iter
        (fun (tag,re) ->
           Db.exec_no_cursor db
             {|insert into custom_tags values (?,?,?);
             |}
             ~ty:Db.Ty.(p3 text text text)
             self.name tag re
           |> Misc.db_err ~ctx:"prover.to-db.add tag" |> scope.unwrap)
        self.custom;
      ))

let tags_of_db db : _ list =
  try Db.exec_no_params_exn db
        {| select distinct tag from custom_tags ; |}
        ~ty:Db.Ty.(p1 text, id) ~f:Db.Cursor.to_list_rev
  with e ->
    Logs.err
      (fun k->k "cannot find custom tags: %s" (Printexc.to_string e));
    []

let of_db db name : t or_error =
  Misc.err_with
    ~map_err:(Printf.sprintf "while parsing prover %s: %s" name)
    (fun scope ->
       let nonnull s = if s="" then None else Some s in
       let custom =
         try
           Db.exec_exn db
             {| select tag, regex from custom_tags where prover_name=?; |}
             ~ty:Db.Ty.(p1 text, p2 text text, mkp2) ~f:Db.Cursor.to_list
             name
         with e ->
           Logs.err
             (fun k->k "prover.of_db: could not find tags: %s"(Printexc.to_string e));
           []
       in
       Db.exec db
         {|select
            version, binary, unsat, sat, unknown, timeout, memory
           from prover where name=? ; |}
         name
         ~f:Db.Cursor.next
         ~ty:Db.Ty.(p1 text,
                    p2 any_str any_str
                    @>> p5 any_str any_str any_str any_str any_str,
                    fun version binary unsat sat unknown timeout memory ->
                      let version =
                        Version.deser_sexp version |> scope.unwrap
                      in
                      let cmd = "<unknown>" in
                      let sat = nonnull sat in
                      let unsat = nonnull unsat in
                      let unknown = nonnull unknown in
                      let timeout = nonnull timeout in
                      let memory = nonnull memory in
                      { name; cmd; binary_deps=[]; defined_in=None; custom;
                        version; binary;unsat;sat;unknown;timeout;memory})
       |> scope.unwrap_with Db.Rc.to_string
       |> CCOpt.to_result "expected a result"
       |> scope.unwrap
    )

let db_names db : _ list or_error =
  Db.exec_no_params db
    {| select distinct name from prover order by name; " |}
    ~ty:Db.Ty.(p1 text,id) ~f:Db.Cursor.to_list_rev
  |> Misc.db_err ~ctx:"listing provers"

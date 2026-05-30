(* This file is free software. See file "license" for more details. *)

(** {1 Run Prover} *)

open Common

let src_log = Logs.Src.create "prover"

module Log = (val Logs.src_log src_log)

type version = string
type name = string

type t = {
  (* Prover identification *)
  name: name;
  version: version;
  (* Prover execution *)
  binary: string; (* name of the program itself *)
  binary_deps: string list; (* additional list of binaries this depends on *)
  cmd: string;
      (* the command line to run. Possibly contains $binary, $file, $memory and $timeout *)
  produces_proof: bool;
  proof_ext: string option;  (** file extension for proofs *)
  get_checkers:
    (stdout:string ->
    stderr:string ->
    res:Res.t ->
    proof_file:string option ->
    (string * string option) list)
    option;
      (** Given the prover result, return a list of
          [(checker_name, proof_file_override)] pairs. When
          [proof_file_override] is [None] the auto-generated proof file is used;
          when [Some path] that path is used instead (useful when the prover
          writes the proof to a dynamically-chosen location). *)
  (* whether some limits should be enforced/set by ulimit *)
  ulimits: Ulimit.conf;
  (* Result analysis *)
  unsat: string option; (* regex for "unsat" *)
  sat: string option; (* regex for "sat" *)
  unknown: string option; (* regex for "unknown" *)
  timeout: string option; (* regex for "timeout" *)
  memory: string option; (* regex for "out of memory" *)
  custom: (string * string) list; (* custom tags *)
  static_labels: string list;
      (** Labels always attached to every result from this prover *)
}

type t_ = t

let equal p1 p2 = p1.name = p2.name
let name p = p.name
let compare_name = String.compare
let pp_name out p = Fmt.string out p.name
let compare_by_name p1 p2 = compare_name p1.name p2.name

module Version = struct
  type t = version

  let to_string_short v = v
  let pp out v = Fmt.string out v
  let ser_sexp v = v

  let deser_sexp s =
    try
      (* parse old sexp-encoded versions from the DB, e.g. (Tag "foo") *)
      let s = String.trim s in
      if String.length s >= 2 && s.[0] = '(' then (
        let inner = String.sub s 1 (String.length s - 2) |> String.trim in
        if String.starts_with ~prefix:"Tag" inner then
          Scanf.sscanf inner "Tag %S" Fun.id
        else if String.starts_with ~prefix:"Git" inner then
          "<git>"
        else
          s
      ) else
        s
    with _ -> s
end

let pp out self =
  let open Misc.Pp in
  let {
    name;
    version;
    cmd;
    ulimits;
    unsat;
    sat;
    timeout;
    unknown;
    memory;
    binary;
    custom;
    static_labels;
    get_checkers = _;
    produces_proof;
    proof_ext;
    binary_deps = _;
  } =
    self
  in
  Fmt.fprintf out "(@[<hv1>prover%a%a%a%a%a%a%a%a%a%a%a%a%a%a@])"
    (pp_f "name" pp_str) name
    (pp_f "version" Version.pp)
    version (pp_f "cmd" pp_str) cmd (pp_f "binary" pp_str) binary
    (pp_f "ulimit" Ulimit.pp) ulimits (pp_opt "sat" pp_regex) sat
    (pp_opt "unsat" pp_regex) unsat (pp_opt "memory" pp_regex) memory
    (pp_opt "timeout" pp_regex)
    timeout
    (pp_opt "unknown" pp_regex)
    unknown
    (pp_f "produces_proof" Fmt.bool)
    produces_proof
    (pp_opt "proof_ext" pp_str)
    proof_ext
    (pp_l1 (pp_pair pp_str pp_regex))
    custom (pp_l1 pp_str) static_labels

exception Subst_not_found of string
exception Missing_subst_value of string

let subst_aux name = function
  | Some v -> v
  | None -> raise (Missing_subst_value name)

let subst ?binary ?proof_file ?file ?(f = fun _ -> None) () = function
  | "file" as s -> subst_aux s file
  | "proof_file" as s -> subst_aux s proof_file
  | "binary" as s -> subst_aux s binary
  | s ->
    (match f s with
    | Some res -> res
    | None -> raise (Subst_not_found s))

let interpolate_cmd ?(env = [||]) ~subst cmd =
  let buf = Buffer.create 32 in
  let add_str s = Buffer.add_substitute buf subst s in
  Array.iter (fun (key, value) -> add_str (key ^ "=" ^ value ^ " ")) env;
  add_str cmd;
  Buffer.contents buf

let make_command ?env ?proof_file ~limits prover ~file =
  let binary = prover.binary in
  let limit_subst =
    Limit.All.substitute limits ~time_as:Seconds ~memory_as:Megabytes
      ~stack_as:Megabytes
  in
  try
    interpolate_cmd ?env prover.cmd
      ~subst:(subst ~binary ?proof_file ~file ~f:limit_subst ())
  with Subst_not_found s ->
    Error.raise
      (Error.makef "cannot make command for prover %s: cannot find field %s"
         prover.name s)

module Map_name = CCMap.Make (struct
  type t = t_

  let compare = compare_by_name
end)

module As_key = struct
  type t = t_

  let compare p1 p2 =
    let c = String.compare p1.name p2.name in
    if c <> 0 then
      c
    else
      Stdlib.compare p1.version p2.version
end

module Map = CCMap.Make (As_key)
module Set = CCSet.Make (As_key)

let run ?env ?proof_file ~limits ~file (self : t) : Run_proc_result.t =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "prover.run" in
  Trace.add_data_to_span _sp
    [
      "prover", `String self.name;
      "file", `String file;
      ( "timeout",
        `Int
          (CCOpt.map_or ~default:0
             (fun t -> Limit.Time.as_int Seconds t)
             limits.Limit.All.time) );
    ];
  Log.debug (fun k -> k "(@[Prover.run %s %a@])" self.name Limit.All.pp limits);
  (* limits with +1s ulimit padding *)
  let padded_limits =
    Limit.All.update_time (CCOpt.map Limit.Time.(add (mk ~s:1 ()))) limits
  in
  let cmd = make_command ?env ?proof_file ~limits self ~file in
  let cmd = Ulimit.prefix_cmd ~conf:self.ulimits ~limits:padded_limits ~cmd in
  Run_proc.run cmd

let analyze_p_opt (self : t) (r : Run_proc_result.t) :
    (Res.t * string list) option =
  (* find if [re: re option] is present in [stdout] or [stderr] *)
  let find_ re =
    let re = Re.Perl.compile_pat ~opts:[ `Multiline ] re in
    Re.execp re r.stdout || Re.execp re r.stderr
  in
  let find_opt_ re =
    match re with
    | None -> false
    | Some re -> find_ re
  in
  let res_opt =
    if find_opt_ self.sat then
      Some Res.Sat
    else if find_opt_ self.unsat then
      Some Res.Unsat
    else if find_opt_ self.timeout then
      Some Res.Timeout
    else if find_opt_ self.unknown then
      Some Res.Unknown
    else
      CCList.find_map
        (fun (tag, re) ->
          if find_ re then
            Some (Res.Tag tag)
          else
            None)
        self.custom
  in
  CCOpt.map (fun res -> res, []) res_opt

let db_prepare (db : Db.t) : unit =
  Db.exec0 db
    {|
  create table if not exists
    prover (
      name text not null unique,
      version text not null,
      binary blob not null,
      unsat text not null,
      sat text not null,
      unknown text not null,
      timeout text not null,
      memory text not null,
      ulimit_time text not null,
      ulimit_mem text not null,
      ulimit_stack text not null,
      produces_proof bool,
      proof_checker text,
      proof_ext text,
      inherits text
    );

  create table if not exists
    custom_tags (
      prover_name text not null,
      tag text not null,
      regex text not null,
      unique (prover_name,tag) on conflict fail
    );
  |}
  |> Misc.unwrap_db (fun () -> "creating prover table")

let to_db db (self : t) : unit =
  let str_or = CCOpt.get_or ~default:"" in
  Db.exec_no_cursor db
    {|insert into prover values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) on conflict do nothing;
      |}
    ~ty:
      Db.Ty.
        [
          text;
          text;
          blob;
          text;
          text;
          text;
          text;
          text;
          text;
          text;
          text;
          text;
          text;
          text;
          text;
        ]
    self.name
    (Version.ser_sexp self.version)
    self.binary (self.unsat |> str_or) (self.sat |> str_or)
    (self.unknown |> str_or) (self.timeout |> str_or) (self.memory |> str_or)
    (self.ulimits.time |> string_of_bool)
    (self.ulimits.memory |> string_of_bool)
    (self.ulimits.stack |> string_of_bool)
    (self.produces_proof |> string_of_bool)
    ""
    (* proof_checker column kept for schema compat; runtime uses get_checkers *)
    (self.proof_ext |> str_or)
    ""
  (* inherits column kept for schema compatibility, always empty *)
  |> Misc.unwrap_db (fun () -> "prover.to-db");
  if self.custom <> [] then
    List.iter
      (fun (tag, re) ->
        Db.exec_no_cursor db
          {|insert into custom_tags values (?,?,?)
               on conflict do nothing ;
             |}
          ~ty:Db.Ty.(p3 text text text)
          self.name tag re
        |> Misc.unwrap_db (fun () -> "prover.to-db.add-tag"))
      self.custom

let tags_of_db db : _ list =
  if not (Misc.db_has_table db "custom_tags") then
    []
  else (
    try
      Db.exec_no_params_exn db {| select distinct tag from custom_tags ; |}
        ~ty:Db.Ty.(p1 text, id)
        ~f:Db.Cursor.to_list_rev
    with e ->
      Log.err (fun k -> k "cannot find custom tags: %s" (Printexc.to_string e));
      []
  )

let of_db db name : t =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "prover.of-db" in
  Trace.add_data_to_span _sp [ "name", `String name ];
  Error.guard (Error.wrapf "reading prover data for '%s'" name) @@ fun () ->
  let nonnull s =
    if s = "" then
      None
    else
      Some s
  in
  let custom =
    try
      Db.exec_exn db
        {| select tag, regex from custom_tags where prover_name=?; |}
        ~ty:Db.Ty.(p1 text, p2 any_str any_str, mkp2)
        ~f:Db.Cursor.to_list name
    with e ->
      Log.debug (fun k ->
          k "prover.of_db: could not find tags: %s" (Printexc.to_string e));
      []
  in
  let ulimits =
    try
      Db.exec_exn db
        {| select ulimit_time, ulimit_mem, ulimit_stack
                from prover where name=? ; |}
        name ~f:Db.Cursor.get_one_exn
        ~ty:
          Db.Ty.(
            ( p1 text,
              p3 any_str any_str any_str,
              fun time memory stack ->
                let time = bool_of_string time in
                let memory = bool_of_string memory in
                let stack = bool_of_string stack in
                Ulimit.mk ~time ~memory ~stack ))
    with _ ->
      Log.debug (fun k ->
          k "prover.of_db: not ulimit_* fields, assuming defaults");
      { time = true; memory = true; stack = false }
  in
  let produces_proof, proof_ext =
    (* parse separately, for migration purposes (old DBs don't have this) *)
    try
      Db.exec_exn db
        {|select produces_proof, proof_ext from prover where name=?|}
        ~f:Db.Cursor.next
        ~ty:
          Db.Ty.(
            ( [ text ],
              [ nullable text; nullable text ],
              fun a b -> CCOpt.map_or ~default:false bool_of_string a, b ))
        name
      |> CCOpt.get_or ~default:(false, None)
    with _ -> false, None
  in
  Db.exec db
    {|select
            version, binary, unsat, sat, unknown, timeout, memory
           from prover where name=? ; |}
    name ~f:Db.Cursor.next
    ~ty:
      Db.Ty.(
        ( [ text ],
          [ any_str; any_str; any_str; any_str; any_str; any_str; any_str ],
          fun version binary unsat sat unknown timeout memory ->
            let version = Version.deser_sexp version in
            let sat = nonnull sat in
            let unsat = nonnull unsat in
            let unknown = nonnull unknown in
            let timeout = nonnull timeout in
            let memory = nonnull memory in
            {
              name;
              cmd = "<unknown>";
              binary_deps = [];
              static_labels = [];
              custom;
              get_checkers = None;
              produces_proof;
              proof_ext;
              version;
              binary;
              ulimits;
              unsat;
              sat;
              unknown;
              timeout;
              memory;
            } ))
  |> Misc.unwrap_db (fun () -> spf "reading data for prover '%s'" name)
  |> Error.unwrap_opt (spf "no prover by the name '%s'" name)

let db_names db : _ list =
  Db.exec_no_params db {| select distinct name from prover order by name; " |}
    ~ty:Db.Ty.(p1 text, id)
    ~f:Db.Cursor.to_list_rev
  |> Misc.unwrap_db (fun () -> "listing provers")

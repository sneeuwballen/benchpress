(* This file is free software. See file "license" for more details. *)

(** {1 Definitions} *)

open Common
module Str_map = Misc.Str_map
module Log = (val Logs.src_log (Logs.Src.create "benchpress.definition"))

type path = string
type 'a with_loc = 'a With_loc.t

type def =
  | D_prover of Prover.t with_loc
  | D_task of Task.t with_loc
  | D_proof_checker of Proof_checker.t with_loc

type t = {
  defs: def Str_map.t;
  dirs: Dir.t list; (* list of directories *)
  dir_vars: path Str_map.t; (* named directories *)
  errors: Error.t list;
  cur_dir: string; (* for relative paths *)
  config_file: string option;
  tags: string list;
  option_j: int option;
  option_progress: bool option;
}
(** All known definitions *)

let empty : t =
  {
    defs = Str_map.empty;
    dirs = [];
    dir_vars = Str_map.empty;
    cur_dir = Sys.getcwd ();
    tags = [];
    option_j = None;
    option_progress = None;
    config_file = None;
    errors = [];
  }

let add_prover (p : Prover.t with_loc) self : t =
  { self with defs = Str_map.add (Prover.name p.view) (D_prover p) self.defs }

let add_proof_checker (p : Proof_checker.t with_loc) self : t =
  { self with defs = Str_map.add p.view.name (D_proof_checker p) self.defs }

let add_task (t : Task.t with_loc) self : t =
  { self with defs = Str_map.add t.view.Task.name (D_task t) self.defs }

let add_dir (d : Dir.t) self : t =
  let dir_vars =
    match d.name with
    | Some name -> Str_map.add ("dir:" ^ name) d.path self.dir_vars
    | None -> self.dir_vars
  in
  { self with dirs = d :: self.dirs; dir_vars }

let errors self = self.errors
let option_j self = self.option_j
let option_progress self = self.option_progress
let custom_tags self = self.tags

module Def = struct
  type t = def

  let loc = function
    | D_prover p -> p.loc
    | D_task t -> t.loc
    | D_proof_checker p -> p.loc

  let pp out = function
    | D_prover p -> Prover.pp out p.view
    | D_task t -> Task.pp out t.view
    | D_proof_checker p -> Proof_checker.pp out p.view

  let show = Fmt.to_string pp
end

let to_iter self : _ Iter.t = Str_map.to_iter self.defs

let all_provers self : _ list =
  Str_map.values self.defs
  |> Iter.filter_map (function
       | D_prover p -> Some p
       | _ -> None)
  |> Iter.to_rev_list

let all_checkers self : _ list =
  Str_map.values self.defs
  |> Iter.filter_map (function
       | D_proof_checker c -> Some c
       | _ -> None)
  |> Iter.to_rev_list

let all_tasks self : _ list =
  Str_map.values self.defs
  |> Iter.filter_map (function
       | D_task t -> Some t
       | _ -> None)
  |> Iter.to_rev_list

(* compute a version for the prover *)
let get_version ?(binary = "") (v : Stanza.version_field) : Prover.version =
  match v with
  | Stanza.Version_git { dir } ->
    Git { branch = Misc.Git.get_branch dir; commit = Misc.Git.get_commit dir }
  | Stanza.Version_cmd { cmd } ->
    (try
       Tag
         (Misc.get_cmd_out
         @@ Prover.interpolate_cmd cmd ~subst:(Prover.subst ~binary ()))
     with Prover.Subst_not_found s ->
       Tag (Printf.sprintf "command `%s` failed: cannot find field %s" cmd s))
  | Stanza.Version_exact v -> v

let find self name = Str_map.get name self.defs

let find_prover self name : Prover.t with_loc =
  match Str_map.get name self.defs with
  | Some (D_prover p) -> p
  | Some _ -> Error.failf "%S is not a prover" name
  | _ -> Error.failf "prover %S is not defined" name

let find_checker self name : Proof_checker.t with_loc =
  match Str_map.get name self.defs with
  | Some (D_proof_checker p) -> p
  | Some _ -> Error.failf "%S is not a proof checker" name
  | _ -> Error.failf "proof checker %S is not defined" name

let find_task self name : Task.t with_loc =
  match Str_map.get name self.defs with
  | Some (D_task p) -> p
  | Some _ -> Error.failf "%S is not a task" name
  | _ -> Error.failf "task %S is not defined" name

let find_prover' self name = With_loc.view @@ find_prover self name
let find_task' self name = With_loc.view @@ find_task self name

let norm_path ?(dir_vars = Str_map.empty) ~cur_dir s =
  let f s =
    match s with
    | "cur_dir" -> Some cur_dir
    | _ -> Str_map.find_opt s dir_vars
  in
  s |> Xdg.interpolate_home ~f |> Misc.mk_abs_path

(* find a known directory for [path] *)
let mk_subdir self path : Subdir.t =
  let path = norm_path ~dir_vars:self.dir_vars ~cur_dir:self.cur_dir path in
  (* helper *)
  let is_parent (dir : string) (f : string) : bool =
    let fd_dir = (Unix.stat dir).Unix.st_dev in
    let same_file f =
      try (Unix.stat f).Unix.st_dev = fd_dir with _ -> false
    in
    (* check f and its parents *)
    let rec check f =
      same_file f
      ||
      let parent = Filename.dirname f in
      parent <> f && check parent
    in
    check f
  in
  CCList.find_map
    (fun dir ->
      Logs.debug (fun k -> k "check prefix dir=%S for %S" dir.Dir.path path);
      if is_parent dir.Dir.path path then
        Some { Subdir.path; loc = dir.loc; inside = dir }
      else
        None)
    self.dirs
  |> Error.unwrap_opt' (fun () ->
         spf "no known directory contains path %S" path)

let rec conv_expect self = function
  | Stanza.E_const r -> Dir.E_const r
  | Stanza.E_program { prover } ->
    let p = find_prover self prover in
    Dir.E_program { prover = p.view }
  | Stanza.E_try l ->
    let l = CCList.map (conv_expect self) l in
    Dir.E_try l

let mk_limits ?timeout ?memory ?stack () =
  (* Timeouts are expressed in seconds in the config files *)
  let time = CCOpt.map (fun s -> Limit.Time.mk ~s ()) timeout in
  (* Memory limits are expressed in Megabytes in the config files *)
  let memory = CCOpt.map (fun m -> Limit.Memory.mk ~m ()) memory in
  (* Stack sizes are also given in Megabytes *)
  let stack =
    CCOpt.map
      (function
        | Stanza.Unlimited -> Limit.Stack.Unlimited
        | Stanza.Limited m -> Limit.Stack.Limited (Limit.Memory.mk ~m ()))
      stack
  in
  Limit.All.mk ?time ?memory ?stack ()

(* Add lines from [files] to [dirs] *)
let mk_paths ?(dir_files = []) dirs =
  List.fold_left
    (fun dirs f ->
      let f_lines = CCIO.with_in f CCIO.read_lines_l in
      List.rev_append f_lines dirs) dirs dir_files

let mk_run_provers ?j ?timeout ?memory ?stack ?pattern ~paths ~provers ~loc
    (self : t) : Action.run_provers =
  let provers = CCList.map (find_prover' self) provers in
  let dirs = CCList.map (mk_subdir self) paths in
  let limits = mk_limits ?timeout ?memory ?stack () in
  Action.{ j; limits; dirs; provers; pattern; loc }

let mk_run_provers_slurm_submission ?j ~paths ?timeout ?memory ?stack ?pattern
    ~provers ?loc ?partition ?nodes ?addr ?port ?ntasks (self : t) :
    Action.run_provers_slurm_submission =
  let ge_val opt min def =
    match opt with
    | Some v when v >= min -> v
    | _ -> def
  in
  let provers = CCList.map (find_prover' self) provers in
  let dirs = CCList.map (mk_subdir self) paths in
  let limits = mk_limits ?timeout ?memory ?stack () in
  let nodes = ge_val nodes 1 1 in
  let addr = CCOpt.value addr ~default:(Misc.localhost_addr ()) in
  let port = ge_val port 0 0 in
  let j =
    match j with
    | Some v when v > 0 -> j
    | _ -> None
  in
  let ntasks = ge_val ntasks 1 10 in
  {
    partition;
    nodes;
    j;
    addr;
    port;
    ntasks;
    provers;
    dirs;
    pattern;
    limits;
    loc;
  }

let rec mk_action (self : t) (a : Stanza.action) : _ =
  match a with
  | Stanza.A_run_provers
      { provers; memory; dirs; dir_files; timeout; stack; pattern; j; loc } ->
    let paths = mk_paths ~dir_files dirs in
    let a =
      mk_run_provers ?j ?timeout ?memory ?stack ?pattern ~loc:(Some loc)
        ~paths ~provers self
    in
    Action.Act_run_provers a
  | Stanza.A_run_provers_slurm
      {
        provers;
        memory;
        dirs;
        dir_files;
        timeout;
        stack;
        pattern;
        j;
        partition;
        nodes;
        addr;
        port;
        ntasks;
        loc;
      } ->
    let paths = mk_paths ~dir_files dirs in
    let a =
      mk_run_provers_slurm_submission ?j ?timeout ?memory ?stack ?pattern ~loc
        ~paths ~provers ?partition ?nodes ?addr ?port ?ntasks self
    in
    Action.Act_run_slurm_submission a
  | Stanza.A_progn l ->
    let l = CCList.map (mk_action self) l in
    Action.Act_progn l
  | Stanza.A_git_checkout { dir; ref; fetch_first; loc } ->
    let dir = norm_path ~cur_dir:self.cur_dir dir in
    if Sys.file_exists dir && Sys.is_directory dir then (
      let fetch_first =
        CCOpt.map
          (function
            | Stanza.GF_fetch -> Action.Git_fetch
            | GF_pull -> Action.Git_pull)
          fetch_first
      in
      Action.Act_git_checkout { dir; ref; loc; fetch_first }
    ) else
      Error.failf ~loc "%s is not an existing directory" (Filename.quote dir)
  | Stanza.A_run_cmd { cmd; loc } -> Action.Act_run_cmd { cmd; loc }

let str_mem a b = CCString.mem ~sub:a b

(* conversion from stanzas *)
let add_stanza_ (st : Stanza.t) self : t =
  Logs.info (fun k -> k "add-stanza %a" Stanza.pp st);
  let open Stanza in
  match st with
  | St_enter_file file ->
    {
      self with
      cur_dir = Misc.mk_abs_path (Filename.dirname file);
      config_file = Some file;
    }
  | St_dir { name; path; expect; pattern; loc } ->
    Log.debug (fun k -> k "cur dir: '%s'" self.cur_dir);
    let path = norm_path ~dir_vars:self.dir_vars ~cur_dir:self.cur_dir path in
    if Sys.file_exists path && Sys.is_directory path then
      ()
    else
      Error.failf ~loc "%S is not a directory (cur_dir: %S)" path self.cur_dir;
    let expect =
      match expect with
      | None -> Dir.E_comment
      | Some e -> conv_expect self e
    in
    let d = { Dir.name; path; expect; loc; pattern } in
    add_dir d self
  | St_prover
      {
        name;
        binary;
        cmd;
        sat;
        unsat;
        timeout;
        unknown;
        memory;
        version;
        custom;
        ulimits;
        loc;
        produces_proof;
        proof_ext;
        proof_checker;
        inherits;
      } ->
    (* add prover *)
    let inherits_p =
      match inherits with
      | None -> None
      | Some s ->
        let p' = find_prover' self s in
        Some p'
    in

    let cmd =
      match cmd, inherits_p with
      | Some c, _ -> c
      | None, Some p -> p.cmd
      | None, None -> Error.failf ~loc "needs 'inherits' or 'cmd'"
    in
    let cmd = Misc.str_replace [ "cur_dir", self.cur_dir ] cmd in

    let binary =
      match binary, inherits_p with
      | Some b, _ ->
        if not (str_mem "$binary" cmd) then
          Error.failf ~loc "Prover's `cmd` does not contain $binary";
        Misc.str_replace ["cur_dir", self.cur_dir] b
      | None, Some p ->
        if str_mem "$binary" cmd then
          p.binary
        else
          Misc.get_binary_of_cmd cmd
      | None, None ->
        (* Don't outright forbid a command using "$binary" without a binary
           field, because that is useful to define "template" provers. But
           we also can't use [get_binary_of_cmd] because that would probably
           return "$binary", which would get confusing. So we put in a
           (probably) non-existent binary instead *)
        if str_mem "$binary" cmd then begin
          Log.warn (fun m ->
            m "Prover's `cmd` uses $binary, but the prover has no binary");
          "benchpress-no-prover-binary"
        end else
          Misc.get_binary_of_cmd cmd
    in

    let sat =
      match sat, inherits_p with
      | None, Some p -> p.sat
      | r, _ -> r
    and unsat =
      match unsat, inherits_p with
      | None, Some p -> p.unsat
      | r, _ -> r
    and unknown =
      match unknown, inherits_p with
      | None, Some p -> p.unknown
      | r, _ -> r
    and timeout =
      match timeout, inherits_p with
      | None, Some p -> p.timeout
      | r, _ -> r
    and memory =
      match memory, inherits_p with
      | None, Some p -> p.memory
      | r, _ -> r
    and produces_proof =
      match produces_proof, inherits_p with
      | None, Some p -> p.produces_proof
      | Some b, _ -> b
      | _ -> false
    and proof_ext =
      match proof_ext, inherits_p with
      | None, Some p -> p.proof_ext
      | r, _ -> r
    and proof_checker =
      match proof_checker, inherits_p with
      | None, Some p -> p.proof_checker
      | r, _ -> r
    in
    if not (str_mem "$file" cmd) then
      Error.failf ~loc "Prover's `cmd` does not contain $file";
    if produces_proof && not (str_mem "$proof_file" cmd) then
      Error.failf ~loc
        "Prover produces proof, but `cmd` does not contain $proof_file";
    if produces_proof then (
      match proof_checker with
      | None ->
        Error.failf ~loc "Prover produces proof, but no checker is declared"
      | Some c ->
        (* make sure it's defined *)
        (try
           let _c = find_checker self c in
           ()
         with _ -> Error.failf ~loc "No proof checker named '%s' found" c)
    );
    if (not produces_proof) && str_mem "$proof_file" cmd then
      Error.failf ~loc
        "Prover does not produce proof, but `cmd` does contains $proof_file.\n\
         It will not be substituted.";
    let version =
      match version with
      | Some v -> v
      | None -> Version_exact (Prover.Tag "<unknown>")
    in
    let ulimits =
      match ulimits, inherits_p with
      | Some l, _ -> l
      | None, Some p -> p.ulimits
      | None, None -> Ulimit.mk ~time:true ~memory:true ~stack:false
    in
    let p =
      {
        Prover.name;
        cmd;
        sat;
        unsat;
        timeout;
        unknown;
        memory;
        ulimits;
        binary;
        binary_deps = [];
        version = get_version ~binary version;
        custom;
        defined_in = self.config_file;
        inherits;
        produces_proof;
        proof_ext;
        proof_checker;
      }
    in
    add_prover (With_loc.make ~loc p) self
  | St_proof_checker { name; cmd; valid; invalid; loc } ->
    let cmd = Misc.str_replace [ "cur_dir", self.cur_dir ] cmd in
    let pc = { Proof_checker.name; cmd; valid; invalid } in
    add_proof_checker (With_loc.make ~loc pc) self
  | St_task { name; synopsis; action; loc } ->
    let action = mk_action self action in
    let t = { Task.name; synopsis; action; defined_in = self.config_file } in
    add_task (With_loc.make ~loc t) self
  | St_set_options { progress; j; loc = _ } ->
    let open CCOpt.Infix in
    {
      self with
      option_j = j <+> self.option_j;
      option_progress = progress <+> self.option_progress;
    }
  | St_declare_custom_tag { tag = t; loc } ->
    if List.mem t self.tags then
      Error.failf ~loc "tag %s already declared" t
    else
      { self with tags = t :: self.tags }
  | St_error { err; loc = _ } -> { self with errors = err :: self.errors }

let add_stanza ?(reify_errors = false) st self : t =
  if reify_errors then (
    try add_stanza_ st self
    with Error.E e -> { self with errors = e :: self.errors }
  ) else
    add_stanza_ st self

let add_stanza_l ?reify_errors (l : Stanza.t list) self : t =
  List.fold_left (fun self st -> add_stanza ?reify_errors st self) self l

let of_stanza_l ?reify_errors l = add_stanza_l ?reify_errors l empty

let completions (self : t) ?before_pos (str : string) : def list =
  to_iter self
  |> Iter.filter_map (fun (name, d) ->
         if CCString.prefix ~pre:str name then
           Some d
         else
           None)
  |> (match before_pos with
     | None -> fun i -> i
     | Some query_pos ->
       Iter.filter (fun d ->
           (* keep [d] if it comes before [query_pos] *)
           let loc = Def.loc d in
           Loc.Pos.le loc.input loc.stop query_pos))
  |> Iter.to_rev_list

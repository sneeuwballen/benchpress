(* This file is free software. See file "license" for more details. *)

open Json_decode
module Log = (val Logs.src_log (Logs.Src.create "benchpress.yaml_config"))

exception Config_error of string

let config_errorf fmt = Printf.ksprintf (fun s -> raise (Config_error s)) fmt

type proof_check_entry = {
  checker: string;
  when_: Predicate.t;
  file: string option;
}

type custom_tag = { tag_name: string; tag_regex: string }

let decode_custom_tag : custom_tag t =
  let* name = field "name" string in
  let+ regex = field "regex" string in
  { tag_name = name; tag_regex = regex }

let decode_proof_check_entry : proof_check_entry t =
  let* checker = field "checker" string in
  let* when_pred =
    field_or "when" ~default:Predicate.Always
      (let* s = string in
       match Predicate.parse s with
       | Ok t -> return t
       | Error e -> failf "invalid predicate: %s" e)
  in
  let+ file = field_opt "file" string in
  { checker; when_ = when_pred; file }

let make_get_checkers (entries : proof_check_entry list) ~(res : Res.t)
    ~(proof_file : string option) : (string * string option) list =
  let ctx =
    { Predicate.result = Res.to_string res; has_proof = proof_file <> None }
  in
  List.filter_map
    (fun e ->
      if Predicate.eval e.when_ ctx then
        Some (e.checker, e.file)
      else
        None)
    entries

let decode_proof_checker (cur_dir : string) : Proof_checker.t t =
  let* name = field "name" string in
  let* cmd = field "cmd" string >|= Misc.str_replace [ "cur_dir", cur_dir ] in
  let* valid = field "valid" string in
  let+ invalid = field "invalid" string in
  { Proof_checker.name; cmd; valid; invalid }

(** {2 Version} *)

type raw_version =
  | RV_string of string
  | RV_git of string  (** directory to run git in *)
  | RV_cmd of string  (** command to run *)

let decode_version : raw_version t =
  let open Json_decode in
  let* v = value in
  match v.node with
  | String s -> return (RV_string s)
  | O _ ->
    let* git = field_opt "git" string in
    let* cmd = field_opt "cmd" string in
    let+ exact = field_opt "exact" string in
    (match git, cmd, exact with
    | Some dir, None, None -> RV_git dir
    | None, Some cmd, None -> RV_cmd cmd
    | None, None, Some s -> RV_string s
    | _ -> config_errorf "version: use exactly one of 'git', 'cmd', or 'exact'")
  | _ -> failf "expected a string or object for version"

let resolve_version (v : raw_version) ~(binary : string) : string =
  match v with
  | RV_string s -> s
  | RV_git dir ->
    (try
       let branch = Misc.Git.get_branch dir in
       let commit = Misc.Git.get_commit dir in
       Printf.sprintf "%s#%s" branch commit
     with _ -> Printf.sprintf "<git-error in %s>" dir)
  | RV_cmd cmd ->
    let cmd = Misc.str_replace [ "binary", binary ] cmd in
    (try Misc.get_cmd_out cmd
     with _ -> Printf.sprintf "command '%s' failed" cmd)

(** {2 Prover (raw and resolved)} *)

type raw_prover = {
  rp_name: string;
  rp_extends: string option;
  rp_binary: string option;
  rp_cmd: string option;
  rp_version: raw_version option;
  rp_sat: string option;
  rp_unsat: string option;
  rp_unknown: string option;
  rp_timeout: string option;
  rp_memory: string option;
  rp_produces_proof: bool option;
  rp_proof_ext: string option;
  rp_binary_deps: string list option;
  rp_static_labels: string list option;
  rp_ulimit_time: bool option;
  rp_ulimit_memory: bool option;
  rp_ulimit_stack: bool option;
  rp_tags: custom_tag list option;
  rp_proof_check: proof_check_entry list option;
}

let decode_raw_prover (_cur_dir : string) : raw_prover t =
  let* name = field "name" string in
  let* extends = field_opt "extends" string in
  let* binary = field_opt "binary" string in
  let* cmd = field_opt "cmd" string in
  let* version = field_opt "version" decode_version in
  let* sat = field_opt "sat" string in
  let* unsat = field_opt "unsat" string in
  let* unknown = field_opt "unknown" string in
  let* timeout = field_opt "timeout" string in
  let* memory = field_opt "memory" string in
  let* produces_proof = field_opt "produces_proof" bool in
  let* proof_ext = field_opt "proof_ext" string in
  let* binary_deps = field_opt "binary_deps" (list string) in
  let* static_labels = field_opt "static_labels" (list string) in
  let* ulimit_time = field_opt "ulimit_time" bool in
  let* ulimit_memory = field_opt "ulimit_memory" bool in
  let* ulimit_stack = field_opt "ulimit_stack" bool in
  let* tags = field_opt "tags" (list decode_custom_tag) in
  let+ proof_check = field_opt "proof_check" (list decode_proof_check_entry) in
  {
    rp_name = name;
    rp_extends = extends;
    rp_binary = binary;
    rp_cmd = cmd;
    rp_version = version;
    rp_sat = sat;
    rp_unsat = unsat;
    rp_unknown = unknown;
    rp_timeout = timeout;
    rp_memory = memory;
    rp_produces_proof = produces_proof;
    rp_proof_ext = proof_ext;
    rp_binary_deps = binary_deps;
    rp_static_labels = static_labels;
    rp_ulimit_time = ulimit_time;
    rp_ulimit_memory = ulimit_memory;
    rp_ulimit_stack = ulimit_stack;
    rp_tags = tags;
    rp_proof_check = proof_check;
  }

let resolve_prover (rp : raw_prover) ~(base : Prover.t option)
    ~(cur_dir : string) : Prover.t =
  let name = rp.rp_name in
  let cmd =
    match rp.rp_cmd with
    | Some c -> c
    | None ->
      (match base with
      | Some b -> b.Prover.cmd
      | None -> config_errorf "prover '%s': requires 'cmd' or 'extends'" name)
  in
  let binary =
    (match rp.rp_binary with
    | Some b -> b
    | None ->
      (match base with
      | Some b -> b.Prover.binary
      | None -> name))
    |> Misc.str_replace [ "cur_dir", cur_dir ]
  in
  let cmd = Misc.str_replace [ "cur_dir", cur_dir ] cmd in
  let version =
    match rp.rp_version with
    | Some v -> resolve_version v ~binary
    | None ->
      (match base with
      | Some b -> b.Prover.version
      | None -> "<unknown>")
  in
  let sat =
    match rp.rp_sat with
    | Some _ -> rp.rp_sat
    | None ->
      (match base with
      | Some b -> b.Prover.sat
      | None -> None)
  in
  let unsat =
    match rp.rp_unsat with
    | Some _ -> rp.rp_unsat
    | None ->
      (match base with
      | Some b -> b.Prover.unsat
      | None -> None)
  in
  let unknown =
    match rp.rp_unknown with
    | Some _ -> rp.rp_unknown
    | None ->
      (match base with
      | Some b -> b.Prover.unknown
      | None -> None)
  in
  let timeout =
    match rp.rp_timeout with
    | Some _ -> rp.rp_timeout
    | None ->
      (match base with
      | Some b -> b.Prover.timeout
      | None -> None)
  in
  let memory =
    match rp.rp_memory with
    | Some _ -> rp.rp_memory
    | None ->
      (match base with
      | Some b -> b.Prover.memory
      | None -> None)
  in
  let produces_proof =
    match rp.rp_produces_proof with
    | Some x -> x
    | None ->
      (match base with
      | Some b -> b.Prover.produces_proof
      | None -> false)
  in
  let proof_ext =
    match rp.rp_proof_ext with
    | Some _ -> rp.rp_proof_ext
    | None ->
      (match base with
      | Some b -> b.Prover.proof_ext
      | None -> None)
  in
  let binary_deps =
    match rp.rp_binary_deps with
    | Some x -> x
    | None ->
      (match base with
      | Some b -> b.Prover.binary_deps
      | None -> [])
  in
  let static_labels =
    match rp.rp_static_labels with
    | Some x -> x
    | None ->
      (match base with
      | Some b -> b.Prover.static_labels
      | None -> [])
  in
  let ulimit_time =
    match rp.rp_ulimit_time with
    | Some x -> x
    | None ->
      (match base with
      | Some b -> b.Prover.ulimits.time
      | None -> true)
  in
  let ulimit_memory =
    match rp.rp_ulimit_memory with
    | Some x -> x
    | None ->
      (match base with
      | Some b -> b.Prover.ulimits.memory
      | None -> true)
  in
  let ulimit_stack =
    match rp.rp_ulimit_stack with
    | Some x -> x
    | None ->
      (match base with
      | Some b -> b.Prover.ulimits.stack
      | None -> false)
  in
  let tags =
    match rp.rp_tags with
    | Some x -> x
    | None ->
      (match base with
      | Some b ->
        List.map (fun (n, r) -> { tag_name = n; tag_regex = r }) b.Prover.custom
      | None -> [])
  in
  let proof_check =
    match rp.rp_proof_check with
    | Some x -> x
    | None ->
      (match base with
      | Some b ->
        (match b.Prover.get_checkers with
        | None -> []
        | Some _ ->
          config_errorf
            "prover '%s': extends a prover with dynamic get_checkers; must \
             specify proof_check explicitly"
            name)
      | None -> [])
  in
  let custom =
    List.map (fun (t : custom_tag) -> t.tag_name, t.tag_regex) tags
  in
  let get_checkers =
    if proof_check = [] then
      None
    else
      Some
        (fun ~stdout:_ ~stderr:_ ~res ~proof_file ->
          make_get_checkers proof_check ~res ~proof_file)
  in
  {
    Prover.name;
    binary;
    cmd;
    binary_deps;
    version;
    produces_proof;
    proof_ext;
    get_checkers;
    ulimits =
      Ulimit.mk ~time:ulimit_time ~memory:ulimit_memory ~stack:ulimit_stack;
    unsat;
    sat;
    unknown;
    timeout;
    memory;
    custom;
    static_labels;
  }

(** {2 Dir (raw and resolved)} *)

type raw_expect =
  | RE_string of string
  | RE_try of raw_expect list
  | RE_run of string  (** prover name *)

let decode_raw_expect : raw_expect t =
  fix (fun self ->
      let* v = value in
      match v.node with
      | String s -> return (RE_string s)
      | A _ ->
        let+ items = list self in
        RE_try items
      | O _ ->
        let* try_ = field_opt "try" (list self) in
        let+ run = field_opt "run" string in
        (match try_, run with
        | Some l, None -> RE_try l
        | None, Some name -> RE_run name
        | Some _, Some _ ->
          config_errorf "expect: cannot have both 'try' and 'run'"
        | None, None -> config_errorf "expect: object must have 'try' or 'run'")
      | _ -> failf "expected string, list, or object for expect")

let parse_expect_string = function
  | "comment" -> `Const Dir.E_comment
  | "unknown" -> `Const (Dir.E_const Res.Unknown)
  | "sat" -> `Const (Dir.E_const Res.Sat)
  | "unsat" -> `Const (Dir.E_const Res.Unsat)
  | "error" -> `Const (Dir.E_const Res.Error)
  | "timeout" -> `Const (Dir.E_const Res.Timeout)
  | _ -> `Const Dir.E_comment

let rec resolve_expect (re : raw_expect) ~(defs : Definitions.t) : Dir.expect =
  match re with
  | RE_string s ->
    (match parse_expect_string s with
    | `Const e -> e
    | _ -> Dir.E_comment)
  | RE_run name -> Dir.E_program { prover = Definitions.find_prover' defs name }
  | RE_try l ->
    let l = List.map (fun re -> resolve_expect re ~defs) l in
    Dir.E_try l

type raw_dir = {
  rd_name: string option;
  rd_extends: string option;
  rd_path: string option;
  rd_expect: raw_expect option;
  rd_pattern: string option;
}

let decode_raw_dir (_cur_dir : string) : raw_dir t =
  let* name = field_opt "name" string in
  let* extends = field_opt "extends" string in
  let* path = field_opt "path" string in
  let* expect = field_opt "expect" decode_raw_expect in
  let+ pattern = field_opt "pattern" string in
  {
    rd_name = name;
    rd_extends = extends;
    rd_path = path;
    rd_expect = expect;
    rd_pattern = pattern;
  }

let resolve_dir (rd : raw_dir) ~(base : Dir.t option) ~(defs : Definitions.t)
    ~(cur_dir : string) : Dir.t =
  let name =
    match rd.rd_name with
    | Some _ -> rd.rd_name
    | None ->
      (match base with
      | Some b -> b.Dir.name
      | None -> None)
  in
  let path =
    match rd.rd_path with
    | Some p -> p
    | None ->
      (match base with
      | Some b -> b.Dir.path
      | None ->
        config_errorf "%s"
          (match name with
          | Some n -> Printf.sprintf "dir '%s': requires 'path' or 'extends'" n
          | None -> "dir: requires 'path' or 'extends'"))
  in
  let path =
    path
    |> Misc.str_replace [ "cur_dir", cur_dir ]
    |> Xdg.interpolate_home |> Misc.mk_abs_path
  in
  let pattern =
    match rd.rd_pattern with
    | Some _ -> rd.rd_pattern
    | None ->
      (match base with
      | Some b -> b.Dir.pattern
      | None -> None)
  in
  let expect =
    match rd.rd_expect with
    | Some re -> resolve_expect ~defs re
    | None ->
      (match base with
      | Some b -> b.Dir.expect
      | None -> Dir.E_comment)
  in
  { Dir.name; path; expect; pattern; loc = Loc.none }

(** {2 Actions} *)

let parse_git_fetch = function
  | "fetch" -> Action.Git_fetch
  | "pull" -> Action.Git_pull
  | s ->
    config_errorf
      "git_checkout.fetch_first: expected 'fetch' or 'pull', got '%s'" s

let rec resolve_action ~(defs : Definitions.t) (action_val : Config_value.value)
    : Action.t =
  let open Json_decode in
  let find_prover name = Definitions.find_prover' defs name in
  match action_val.node with
  | O ((k, _) :: _) ->
    (match k with
    | "run_provers" ->
      let inner = run_exn (field "run_provers" value) action_val in
      let prover_names = run_exn (field "provers" (list string)) inner in
      let dir_paths =
        run_exn (field_or "dirs" ~default:[] (list string)) inner
      in
      let timeout = run_exn (field_opt "timeout" int) inner in
      let memory = run_exn (field_opt "memory" int) inner in
      let j = run_exn (field_opt "j" int) inner in
      let pattern = run_exn (field_opt "pattern" string) inner in
      let provers = List.map find_prover prover_names in
      let dirs = List.map (Definitions.mk_subdir defs) dir_paths in
      let limits = Definitions.mk_limits ?timeout ?memory () in
      Action.Act_run_provers
        { j; dirs; provers; pattern; limits; loc = Some Loc.none }
    | "run_provers_slurm" ->
      let inner = run_exn (field "run_provers_slurm" value) action_val in
      let prover_names = run_exn (field "provers" (list string)) inner in
      let dir_paths =
        run_exn (field_or "dirs" ~default:[] (list string)) inner
      in
      let timeout = run_exn (field_opt "timeout" int) inner in
      let memory = run_exn (field_opt "memory" int) inner in
      let j = run_exn (field_opt "j" int) inner in
      let pattern = run_exn (field_opt "pattern" string) inner in
      let partition = run_exn (field_opt "partition" string) inner in
      let nodes = run_exn (field_opt "nodes" int) inner in
      let addr_str = run_exn (field_opt "addr" string) inner in
      let port = run_exn (field_opt "port" int) inner in
      let ntasks = run_exn (field_opt "ntasks" int) inner in
      let provers = List.map find_prover prover_names in
      let dirs = List.map (Definitions.mk_subdir defs) dir_paths in
      let limits = Definitions.mk_limits ?timeout ?memory () in
      let nodes = CCOption.value nodes ~default:1 in
      let addr =
        match addr_str with
        | Some s ->
          (try Unix.inet_addr_of_string s
           with _ -> config_errorf "invalid addr '%s'" s)
        | None -> Misc.localhost_addr ()
      in
      let port = CCOption.value port ~default:0 in
      let ntasks = CCOption.value ntasks ~default:10 in
      Action.Act_run_slurm_submission
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
          loc = Some Loc.none;
        }
    | "git_checkout" ->
      let inner = run_exn (field "git_checkout" value) action_val in
      let dir = run_exn (field "dir" string) inner in
      let ref = run_exn (field "ref" string) inner in
      let fetch_first = run_exn (field_opt "fetch_first" string) inner in
      let fetch_first = CCOption.map parse_git_fetch fetch_first in
      Action.Act_git_checkout { dir; ref; fetch_first; loc = Loc.none }
    | "seq" ->
      let actions = run_exn (field "seq" (list value)) action_val in
      Action.Act_progn (List.map (fun v -> resolve_action ~defs v) actions)
    | "run_cmd" ->
      let cmd = run_exn (field "run_cmd" string) action_val in
      Action.Act_run_cmd { cmd; loc = Loc.none }
    | k' -> config_errorf "unknown action kind '%s'" k')
  | _ -> config_errorf "expected an action object"

(** {2 Task and Options} *)

let decode_task_raw : (string * Config_value.value) t =
  let* name = field "name" string in
  let+ action_v = field "action" value in
  name, action_v

type nats_config = Nats_absent | Nats_disabled | Nats_server of string

let decode_nats : nats_config t =
  let open Json_decode in
  let* v = value in
  match v.node with
  | String s -> return (Nats_server s)
  | O _ ->
    let* enabled = field_opt "enabled" bool in
    let+ server = field_opt "server" string in
    (match enabled with
    | Some false -> Nats_disabled
    | _ -> Nats_server (Option.value ~default:"localhost:4222" server))
  | _ -> failf "expected a string or object for nats"

let decode_options : (int option * bool option * nats_config) t =
  let* j = field_opt "j" int in
  let* progress = field_opt "progress" bool in
  let+ nats_opt = field_opt "nats" decode_nats in
  let nats = Option.value ~default:Nats_absent nats_opt in
  j, progress, nats

(** {2 Main Decode} *)

let decode ~previous (value : Config_value.value) (cur_dir : string) :
    Definitions.t =
  let decoder =
    let* raw_provers =
      field_or "provers" ~default:[] (list (decode_raw_prover cur_dir))
    in
    let* checkers =
      field_or "proof_checkers" ~default:[]
        (list (decode_proof_checker cur_dir))
    in
    let* raw_dirs =
      field_or "dirs" ~default:[] (list (decode_raw_dir cur_dir))
    in
    let* raw_tasks = field_or "tasks" ~default:[] (list decode_task_raw) in
    let+ opts = field_opt "options" decode_options in
    raw_provers, checkers, raw_dirs, raw_tasks, opts
  in
  match run decoder value with
  | Ok (raw_provers, checkers, raw_dirs, raw_tasks, opts) ->
    let resolve_provers ~defs raw_provers =
      let tbl = Hashtbl.create 32 in
      (* seed with provers from previous configs for cross-file extends *)
      List.iter
        (fun p ->
          Hashtbl.add tbl (Prover.name (With_loc.view p)) (With_loc.view p))
        (Definitions.all_provers defs);
      List.iter
        (fun (rp : raw_prover) ->
          let base =
            match rp.rp_extends with
            | None -> None
            | Some base_name ->
              (match Hashtbl.find_opt tbl base_name with
              | Some b -> Some b
              | None ->
                config_errorf
                  "prover '%s': extends unknown prover '%s' (must be declared \
                   before it)"
                  rp.rp_name base_name)
          in
          let p = resolve_prover rp ~base ~cur_dir in
          Hashtbl.add tbl rp.rp_name p)
        raw_provers;
      Hashtbl.to_seq_values tbl |> List.of_seq
    in
    let provers = resolve_provers ~defs:previous raw_provers in
    let checkers = List.map (With_loc.make ~loc:Loc.none) checkers in
    let defs =
      List.fold_left
        (fun defs p ->
          if Definitions.mem_def defs (Prover.name p) then
            Log.warn (fun k ->
                k "duplicate definition '%s', overriding" (Prover.name p));
          Definitions.add_prover (With_loc.make ~loc:Loc.none p) defs)
        previous provers
    in
    let defs =
      List.fold_left
        (fun defs c ->
          let name = (With_loc.view c).Proof_checker.name in
          if Definitions.mem_def defs name then
            Log.warn (fun k -> k "duplicate definition '%s', overriding" name);
          Definitions.add_proof_checker c defs)
        defs checkers
    in
    let resolve_raw_dirs ~defs raw_dirs =
      let tbl = Hashtbl.create 32 in
      (* seed with named dirs from previous configs for cross-file extends *)
      List.iter
        (fun (d : Dir.t) ->
          match d.Dir.name with
          | Some n -> Hashtbl.add tbl n d
          | None -> ())
        (Definitions.all_dirs defs);
      let resolved_dirs = ref [] in
      List.iter
        (fun (rd : raw_dir) ->
          let base =
            match rd.rd_extends with
            | None -> None
            | Some base_name ->
              (match Hashtbl.find_opt tbl base_name with
              | Some b -> Some b
              | None ->
                config_errorf
                  "dir '%s': extends unknown dir '%s' (must be declared before \
                   it)"
                  (CCOption.value rd.rd_name ~default:"<unnamed>")
                  base_name)
          in
          let d = resolve_dir rd ~base ~defs ~cur_dir in
          (match rd.rd_name with
          | Some n -> Hashtbl.add tbl n d
          | None -> ());
          resolved_dirs := d :: !resolved_dirs)
        raw_dirs;
      List.rev !resolved_dirs
    in
    let dirs = resolve_raw_dirs ~defs raw_dirs in
    let defs =
      List.fold_left
        (fun defs d ->
          (match d.Dir.name with
          | Some n when Definitions.mem_dir defs n ->
            Log.warn (fun k -> k "duplicate dir '%s', overriding" n)
          | _ -> ());
          Definitions.add_dir d defs)
        defs dirs
    in
    let defs =
      List.fold_left
        (fun defs (task_name, action_v) ->
          if Definitions.mem_def defs task_name then
            Log.warn (fun k ->
                k "duplicate definition '%s', overriding" task_name);
          let action =
            try resolve_action ~defs action_v
            with Config_error msg ->
              config_errorf "task '%s': %s" task_name msg
          in
          let task = { Task.name = task_name; synopsis = None; action } in
          Definitions.add_task (With_loc.make ~loc:Loc.none task) defs)
        defs raw_tasks
    in
    let opt_j, opt_progress, opt_nats =
      Option.value ~default:(None, None, Nats_absent) opts
    in
    let defs =
      defs
      |> Definitions.with_option_j opt_j
      |> Definitions.with_option_progress opt_progress
    in
    let defs =
      match opt_nats with
      | Nats_absent -> defs
      | Nats_disabled -> Definitions.with_option_nats_server None defs
      | Nats_server s -> Definitions.with_option_nats_server (Some s) defs
    in
    defs |> Definitions.with_cur_dir cur_dir
  | Error e -> config_errorf "%s" (Err.to_string e)

(** {2 Loading} *)

let load_yaml_string ~previous (content : string) ~(cur_dir : string) :
    Definitions.t =
  let file = Filename.concat cur_dir "<config>" in
  match Parse_yaml.parse content ~file with
  | Ok value -> decode ~previous value cur_dir
  | Error msg -> config_errorf "%s" msg

let load_json_string ~previous (content : string) ~(cur_dir : string) :
    Definitions.t =
  let file = Filename.concat cur_dir "<config>" in
  match Parse_json.parse content ~file with
  | Ok value -> decode ~previous value cur_dir
  | Error msg -> config_errorf "%s" msg

let load_file ~previous (path : string) : Definitions.t =
  let content =
    match CCIO.with_in path CCIO.read_all with
    | s -> s
    | exception Sys_error e -> config_errorf "cannot read %s: %s" path e
  in
  let cur_dir = Filename.dirname (Misc.mk_abs_path path) in
  match Filename.extension path with
  | ".yaml" | ".yml" -> load_yaml_string ~previous content ~cur_dir
  | ".json" -> load_json_string ~previous content ~cur_dir
  | ext -> config_errorf "unknown config format: %s" ext

let config_template = Static_data.example_config

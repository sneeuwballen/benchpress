(* This file is free software. See file "license" for more details. *)

open Common
open Json_decode

exception Config_error of string

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

let decode_prover (cur_dir : string) : Prover.t t =
  let* name = field "name" string in
  let* binary = field_or "binary" ~default:name string in
  let* cmd = field "cmd" string in
  let* sat = field_opt "sat" string in
  let* unsat = field_opt "unsat" string in
  let* unknown = field_opt "unknown" string in
  let* timeout = field_opt "timeout" string in
  let* memory = field_opt "memory" string in
  let* produces_proof = field_or "produces_proof" ~default:false bool in
  let* proof_ext = field_opt "proof_ext" string in
  let* version = field_opt "version" string in
  let* binary_deps = field_or "binary_deps" ~default:[] (list string) in
  let* static_labels = field_or "static_labels" ~default:[] (list string) in
  let* ulimit_time = field_or "ulimit_time" ~default:true bool in
  let* ulimit_memory = field_or "ulimit_memory" ~default:true bool in
  let* ulimit_stack = field_or "ulimit_stack" ~default:false bool in
  let* tags = field_or "tags" ~default:[] (list decode_custom_tag) in
  let+ proof_check =
    field_or "proof_check" ~default:[] (list decode_proof_check_entry)
  in
  let binary = Misc.str_replace [ "cur_dir", cur_dir ] binary in
  let cmd = Misc.str_replace [ "cur_dir", cur_dir ] cmd in
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
  let version =
    match version with
    | Some s -> Prover.Tag s
    | None -> Prover.Tag "<unknown>"
  in
  {
    Prover.name;
    binary;
    cmd;
    cmd_fn = None;
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
    analyze_fn = None;
    defined_in = None;
    inherits = None;
  }

let decode_proof_checker (cur_dir : string) : Proof_checker.t t =
  let* name = field "name" string in
  let* cmd = field "cmd" string >|= Misc.str_replace [ "cur_dir", cur_dir ] in
  let* valid = field "valid" string in
  let+ invalid = field "invalid" string in
  { Proof_checker.name; cmd; valid; invalid }

let parse_expect = function
  | "comment" -> Dir.E_comment
  | "unknown" -> Dir.E_const Res.Unknown
  | "sat" -> Dir.E_const Res.Sat
  | "unsat" -> Dir.E_const Res.Unsat
  | "error" -> Dir.E_const Res.Error
  | "timeout" -> Dir.E_const Res.Timeout
  | _ -> Dir.E_comment

let decode_dir (cur_dir : string) : Dir.t t =
  let* name = field_opt "name" string in
  let* path =
    field "path" string
    >|= Misc.str_replace [ "cur_dir", cur_dir ]
    >|= Misc.mk_abs_path
  in
  let* pattern = field_opt "pattern" string in
  let+ expect_str = field_or "expect" ~default:"comment" string in
  let expect = parse_expect expect_str in
  { Dir.name; path; expect; pattern; loc = Loc.none }

let rec resolve_action (defs : Definitions.t) (action_val : Ezjsonm.value) :
    Action.t =
  let open Json_decode in
  match action_val with
  | `O ((k, _) :: _) ->
    (match k with
    | "run_provers" ->
      let prover_names = run_exn (field "provers" (list string)) action_val in
      let dir_paths =
        run_exn (field_or "dirs" ~default:[] (list string)) action_val
      in
      let timeout = run_exn (field_opt "timeout" int) action_val in
      let memory = run_exn (field_opt "memory" int) action_val in
      let j = run_exn (field_opt "j" int) action_val in
      let pattern = run_exn (field_opt "pattern" string) action_val in
      let provers = List.map (Definitions.find_prover' defs) prover_names in
      let dirs = List.map (Definitions.mk_subdir defs) dir_paths in
      let limits = Definitions.mk_limits ?timeout ?memory () in
      Action.Act_run_provers
        { j; dirs; provers; pattern; limits; loc = Some Loc.none }
    | "seq" ->
      let actions = run_exn (field "seq" (list value)) action_val in
      Action.Act_progn (List.map (fun v -> resolve_action defs v) actions)
    | "run_cmd" ->
      let cmd = run_exn (field "run_cmd" string) action_val in
      Action.Act_run_cmd { cmd; loc = Loc.none }
    | k' -> raise (Config_error (Printf.sprintf "unknown action kind '%s'" k')))
  | _ -> raise (Config_error "expected an action object")

let decode_task_raw : (string * Ezjsonm.value) t =
  let* name = field "name" string in
  let+ action_v = field "action" value in
  name, action_v

let decode_options : (int option * bool option) t =
  let* j = field_opt "j" int in
  let+ progress = field_opt "progress" bool in
  j, progress

let decode (value : Ezjsonm.value) (cur_dir : string) : Definitions.t =
  let decoder =
    let* provers =
      field_or "provers" ~default:[] (list (decode_prover cur_dir))
    in
    let* checkers =
      field_or "proof_checkers" ~default:[]
        (list (decode_proof_checker cur_dir))
    in
    let* dirs = field_or "dirs" ~default:[] (list (decode_dir cur_dir)) in
    let* raw_tasks = field_or "tasks" ~default:[] (list decode_task_raw) in
    let+ opts = field_opt "options" decode_options in
    provers, checkers, dirs, raw_tasks, opts
  in
  match run decoder value with
  | Ok (provers, checkers, dirs, raw_tasks, opts) ->
    let provers = List.map (With_loc.make ~loc:Loc.none) provers in
    let checkers = List.map (With_loc.make ~loc:Loc.none) checkers in
    let fold add xs defs = List.fold_left (fun defs x -> add x defs) defs xs in
    let defs =
      Definitions.empty
      |> fold Definitions.add_prover provers
      |> fold Definitions.add_dir dirs
      |> fold Definitions.add_proof_checker checkers
    in
    let defs =
      List.fold_left
        (fun defs (task_name, action_v) ->
          let action =
            try resolve_action defs action_v
            with Config_error msg ->
              raise
                (Config_error (Printf.sprintf "task '%s': %s" task_name msg))
          in
          let task =
            {
              Task.name = task_name;
              synopsis = None;
              action;
              defined_in = None;
            }
          in
          Definitions.add_task (With_loc.make ~loc:Loc.none task) defs)
        defs raw_tasks
    in
    let opt_j, opt_progress =
      match opts with
      | Some (j, p) -> j, p
      | None -> None, None
    in
    defs
    |> Definitions.with_option_j opt_j
    |> Definitions.with_option_progress opt_progress
    |> Definitions.with_cur_dir cur_dir
  | Error e -> raise (Config_error (Err.to_string e))

let load_yaml_string (content : string) (cur_dir : string) : Definitions.t =
  match Yaml.of_string content with
  | Ok value -> decode value cur_dir
  | Error (`Msg e) ->
    raise (Config_error (Printf.sprintf "YAML parse error: %s" e))

let load_json_string (content : string) (cur_dir : string) : Definitions.t =
  let value =
    try Ezjsonm.value_from_string content
    with Ezjsonm.Parse_error (v, msg) ->
      raise (Config_error (Printf.sprintf "JSON parse error: %s" msg))
  in
  decode value cur_dir

let load_file (path : string) : Definitions.t =
  let content =
    match CCIO.with_in path CCIO.read_all with
    | s -> s
    | exception Sys_error e ->
      raise (Config_error (Printf.sprintf "cannot read %s: %s" path e))
  in
  let cur_dir = Filename.dirname (Misc.mk_abs_path path) in
  match Filename.extension path with
  | ".yaml" | ".yml" -> load_yaml_string content cur_dir
  | ".json" -> load_json_string content cur_dir
  | ext -> raise (Config_error (Printf.sprintf "unknown config format: %s" ext))

let config_template =
  {|# yaml-language-server: $schema=https://benchpress.dev/config-schema.json
#
# Benchpress configuration file.
# Write as YAML (preferred) or JSON.
# Use 'benchpress check-config' to validate.

provers:
  # Minimal prover example
  - name: my-prover
    cmd: "$binary $file"
    sat: "^SAT"
    unsat: "^UNSAT"
    unknown: "^UNKNOWN"

  # Prover with proof checking
  # - name: my-prover-with-proof
  #   cmd: "$binary $file $proof_file"
  #   sat: "^SAT"
  #   unsat: "^UNSAT"
  #   produces_proof: true
  #   proof_ext: drat
  #   proof_check:
  #     - checker: drat-trim
  #       when: result == "unsat"

# proof_checkers:
#   - name: drat-trim
#     cmd: "$cur_dir/drat-trim $file $proof_file"
#     valid: "s VERIFIED$"
#     invalid: "s NOT VERIFIED$"

dirs:
  - path: "$cur_dir/problems"
    pattern: '.*\.smt2$'

tasks:
  - name: all
    action:
      run_provers:
        provers: [my-prover]
        timeout: 30
        dirs: ["$cur_dir/problems"]

# options:
#   j: 4
#   progress: true
|}

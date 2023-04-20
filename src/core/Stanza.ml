(** {1 Configuration Stanzas} *)

open Common
module Se = Sexp_loc
module SD = Sexp_decode

type loc = Loc.t

module Log = (val Logs.src_log (Logs.Src.create "benchpress.stanzas"))

(** {2 Type Definitions} *)

(** Result to expect for a problem *)
type expect =
  | E_const of Res.t
  | E_program of { prover: string }
  | E_try of expect list  (** Try these methods successively *)

type version_field =
  | Version_exact of Prover.version
  | Version_git of { dir: string } (* compute by calling git *)
  | Version_cmd of { cmd: string }

type stack_limit = Unlimited | Limited of int

type regex = string
(** A regex in Perl syntax *)

type git_fetch = GF_fetch | GF_pull

type action =
  | A_run_provers of {
      j: int option;
      dirs: string list; (* list of directories to examine *)
      pattern: regex option;
      provers: string list;
      timeout: int option;
      memory: int option;
      stack: stack_limit option;
      loc: Loc.t;
    }
  | A_run_provers_slurm of {
      j: int option;
      dirs: string list; (* list of directories to examine *)
      pattern: regex option;
      provers: string list;
      timeout: int option;
      memory: int option;
      stack: stack_limit option;
      partition: string option;
      nodes: int option;
      addr: Unix.inet_addr option;
      port: int option;
      ntasks: int option;
      loc: Loc.t;
    }
  | A_git_checkout of {
      dir: string;
      ref: string;
      fetch_first: git_fetch option;
      loc: Loc.t;
    }
  | A_run_cmd of { cmd: string; loc: Loc.t }
  | A_progn of action list

(** Stanzas for the configuration *)
type t =
  | St_enter_file of string
  | St_prover of {
      name: string;
      loc: Loc.t;
      version: version_field option;
      cmd: string option;
      produces_proof: bool option;
      proof_ext: string option;
      proof_checker: string option;
      ulimits: Ulimit.conf option;
      (* Result analysis *)
      unsat: regex option;  (** regex for "unsat" *)
      sat: regex option;  (** regex for "sat" *)
      unknown: regex option;  (** regex for "unknown" *)
      timeout: regex option;  (** regex for "timeout" *)
      memory: regex option;  (** regex for "out of memory" *)
      custom: (string * regex) list;  (** regex for custom results *)
      inherits: string option;  (** Inherit another prover definition *)
    }
  | St_proof_checker of {
      name: string;
      loc: Loc.t;
      cmd: string;
      (* results *)
      valid: regex;  (** regex for valid proofs *)
      invalid: regex;  (** regex for invalid proofs *)
    }
  | St_dir of {
      name: string option;
      path: string;
      expect: expect option;
      pattern: regex option;  (** Pattern of problems in this directory *)
      loc: Loc.t;
    }
  | St_task of {
      name: string; (* name of this task *)
      synopsis: string option;
      action: action;
      loc: Loc.t;
    }
  | St_set_options of { progress: bool option; j: int option; loc: Loc.t }
  | St_declare_custom_tag of { tag: string; loc: Loc.t }
  | St_error of { err: Error.t; loc: Loc.t }

let as_error = function
  | St_error { err; loc = _ } -> Some err
  | _ -> None

let errors = CCList.filter_map as_error

(** {2 Printers} *)

let rec pp_expect out = function
  | E_const r -> Fmt.fprintf out "(const %a)" Res.pp r
  | E_program { prover } -> Fmt.fprintf out "(run %s)" prover
  | E_try l -> Fmt.fprintf out "(@[try@ %a@])" (Misc.pp_list pp_expect) l

let pp_version_field out =
  let open Misc.Pp in
  function
  | Version_exact v -> Prover.Version.pp out v
  | Version_git { dir } -> pp_str out @@ Printf.sprintf {|git:%S|} dir
  | Version_cmd { cmd } -> pp_str out @@ Printf.sprintf {|cmd:%S|} cmd

let pp_git_fetch out = function
  | GF_fetch -> Fmt.string out "fetch"
  | GF_pull -> Fmt.string out "pull"

let pp_stack_limit out = function
  | Unlimited -> Fmt.string out "unlimited"
  | Limited i -> Fmt.int out i

let rec pp_action out =
  let open Misc.Pp in
  function
  | A_run_provers { dirs; provers; timeout; memory; stack; pattern; j; loc = _ }
    ->
    Fmt.fprintf out "(@[<v>run_provers%a%a%a%a%a%a%a@])"
      (pp_f "dirs" (pp_l pp_str))
      dirs
      (pp_f "provers" (pp_l pp_str))
      provers
      (pp_opt "pattern" pp_regex)
      pattern (pp_opt "timeout" Fmt.int) timeout (pp_opt "memory" Fmt.int)
      memory
      (pp_opt "stack" pp_stack_limit)
      stack (pp_opt "j" Fmt.int) j
  | A_run_provers_slurm
      {
        dirs;
        provers;
        timeout;
        memory;
        stack;
        pattern;
        j;
        partition;
        nodes;
        addr;
        port;
        ntasks;
        loc = _;
      } ->
    Fmt.fprintf out "(@[<v>run_provers_slurm%a%a%a%a%a%a%a%a%a%a%a%a@])"
      (pp_f "dirs" (pp_l pp_str))
      dirs
      (pp_f "provers" (pp_l pp_str))
      provers
      (pp_opt "pattern" pp_regex)
      pattern (pp_opt "timeout" Fmt.int) timeout (pp_opt "memory" Fmt.int)
      memory
      (pp_opt "stack" pp_stack_limit)
      stack (pp_opt "j" Fmt.int) j
      (pp_opt "partition" Fmt.string)
      partition (pp_opt "nodes" Fmt.int) nodes
      (pp_opt "addr" Misc.pp_inet_addr)
      addr (pp_opt "port" Fmt.int) port (pp_opt "ntasks" Fmt.int) ntasks
  | A_progn l -> Fmt.fprintf out "(@[progn %a@])" (pp_l pp_action) l
  | A_run_cmd { cmd = s; loc = _ } ->
    Fmt.fprintf out "(@[run_cmd %a@])" pp_regex s
  | A_git_checkout { dir; ref; fetch_first; loc = _ } ->
    Fmt.fprintf out "(@[<v>git_checkout%a%a%a@])" (pp_f "dir" pp_regex) dir
      (pp_f "ref" pp_regex) ref
      (pp_opt "fetch-first" pp_git_fetch)
      fetch_first

let pp out =
  let open Misc.Pp in
  function
  | St_enter_file f -> Fmt.fprintf out "(@[enter-file@ %a@])" pp_str f
  | St_dir { name; path; expect; pattern; loc = _ } ->
    Fmt.fprintf out "(@[<v>dir%a%a%a%a@])"
      (pp_opt "name" Fmt.string) name
      (pp_f "path" Fmt.string) path
      (pp_opt "expect" pp_expect)
      expect
      (pp_opt "pattern" pp_regex)
      pattern
  | St_prover
      {
        name;
        cmd;
        version;
        unsat;
        sat;
        unknown;
        timeout;
        memory;
        custom;
        ulimits;
        produces_proof;
        proof_ext;
        proof_checker;
        inherits;
        loc = _;
      } ->
    let pp_custom out (x, y) =
      Fmt.fprintf out "(@[tag %a@ %a@])" pp_str x pp_regex y
    in
    Fmt.fprintf out "(@[<v>prover%a%a%a%a%a%a%a%a%a%a%a%a%a%a@])"
      (pp_f "name" pp_str) name (pp_opt "cmd" pp_str) cmd
      (pp_opt "version" pp_version_field)
      version
      (pp_opt "ulimits" Ulimit.pp)
      ulimits (pp_opt "sat" pp_regex) sat (pp_opt "unsat" pp_regex) unsat
      (pp_opt "unknown" pp_regex)
      unknown
      (pp_opt "timeout" pp_regex)
      timeout (pp_opt "memory" pp_regex) memory (pp_opt "inherits" pp_str)
      inherits
      (pp_opt "produces_proof" Fmt.bool)
      produces_proof
      (pp_opt "proof_checker" pp_str)
      proof_checker
      (pp_opt "proof_ext" pp_str)
      proof_ext (pp_l1 pp_custom) custom
  | St_proof_checker { name; cmd; loc = _; valid; invalid } ->
    Fmt.fprintf out "(@[<hv>proof-checker%a%a%a%a@])" (pp_f "name" pp_str) name
      (pp_f "cmd" pp_str) cmd (pp_f "valid" pp_regex) valid
      (pp_f "invalid" pp_regex) invalid
  | St_task { name; synopsis; action; loc = _ } ->
    Fmt.fprintf out "(@[<v>task%a%a%a@])" (pp_f "name" pp_str) name
      (pp_opt "synopsis" pp_str) synopsis (pp_f "action" pp_action) action
  | St_set_options { j; progress; loc = _ } ->
    Fmt.fprintf out "(@[<v>set-options%a%a])"
      (pp_opt "progress" Fmt.bool)
      progress (pp_opt "j" Fmt.int) j
  | St_declare_custom_tag { tag = t; loc = _ } ->
    Fmt.fprintf out "(custom-tag %s)" t
  | St_error { err; loc = _ } -> Fmt.fprintf out "(@[error %a@])" Error.pp err

let pp_l out l = Fmt.fprintf out "@[<v>%a@]" (Misc.pp_list pp) l

(** {2 Decoding} *)

type tag = string
type state = { mutable tags: tag list; mutable parse_prelude: bool }

let dec_res (st : state) =
  let open SD in
  let* s = atom in
  try return (Res.of_string ~tags:st.tags s)
  with _ -> failf (fun k -> k "expected a `Res.t`, not %S" s)

let dec_regex : regex SD.t =
  let valid_re s =
    try
      ignore (Re.Perl.compile_pat s);
      true
    with _ -> false
  in
  let open SD in
  let* s = atom in
  if valid_re s then
    return s
  else
    fail "expected a valid Perl regex"

let dec_expect tags : _ SD.t =
  let open SD in
  fix @@ fun self ->
  try_l ~msg:"expect stanzas tag (constructors: const|run|try)"
    [
      ( is_applied "const",
        let+ r = applied1 "const" (dec_res tags) in
        E_const r );
      ( is_applied "run",
        let+ prover = applied1 "run" atom in
        E_program { prover } );
      ( is_applied "try",
        let+ e = applied "try" self in
        E_try e );
    ]

let dec_version : _ SD.t =
  let open SD in
  let str =
    atom >>= fun s ->
    return
    @@
    if CCString.prefix ~pre:"git:" s then
      Version_git { dir = snd @@ CCString.Split.left_exn ~by:":" s }
    else if CCString.prefix ~pre:"cmd:" s then
      Version_cmd { cmd = snd @@ CCString.Split.left_exn ~by:":" s }
    else
      Version_exact (Prover.Tag s)
  in
  try_l ~msg:"expected version"
    [
      is_atom, str;
      ( is_applied "git",
        let* m = applied_fields "git" in
        let* branch = Fields.field m "branch" atom in
        let* commit = Fields.field m "commit" atom in
        let+ () = Fields.check_no_field_left m in
        Version_exact (Prover.Git { branch; commit }) );
    ]

let dec_ulimits : _ SD.t =
  let open SD in
  let no_limits = Ulimit.mk ~time:false ~memory:false ~stack:false in
  let none =
    let* s = atom in
    if s = "none" then
      return no_limits
    else
      fail {|expected "none"|}
  in
  let single_limit acc (s : string) =
    match s with
    | "time" -> return { acc with Ulimit.time = true }
    | "memory" -> return { acc with Ulimit.memory = true }
    | "stack" -> return { acc with Ulimit.stack = true }
    | s ->
      failf (fun k ->
          k
            "expected 'ulimit' stanzas (constructors: time|memory|stack, not \
             %S)"
            s)
  in
  try_l ~msg:"expected ulimit"
    [
      is_atom, none;
      ( is_list,
        let* l = list_of ~what:"names of resources to limit" atom in
        fold_l single_limit no_limits l );
    ]

let dec_fetch_first =
  let open SD in
  keyword ~msg:"expected fetch_first" [ "fetch", GF_fetch; "pull", GF_pull ]

let dec_stack_limit : _ SD.t =
  let open SD in
  try_l ~msg:{|expected stack_limit ("unlimited" or an integer)|}
    [
      (try_succeed
      @@ let+ x = int in
         Limited x);
      is_atom, keyword ~msg:"unlimited" [ "unlimited", Unlimited ];
    ]

let dec_action : action SD.t =
  let open SD in
  fix @@ fun self ->
  let* loc = value_loc in
  try_l ~msg:"expected an action"
    [
      ( is_applied "run_provers",
        let* m = applied_fields "run_provers" in
        let* dirs = Fields.field m "dirs" atom_or_atom_list in
        let* provers = Fields.field m "provers" atom_or_atom_list in
        let* j = Fields.field_opt m "j" int in
        let* pattern = Fields.field_opt m "pattern" dec_regex in
        let* timeout = Fields.field_opt m "timeout" int in
        let* memory = Fields.field_opt m "memory" int in
        let* stack = Fields.field_opt m "stack" dec_stack_limit in
        let+ () = Fields.check_no_field_left m in
        let memory = Some (CCOpt.get_or ~default:10_000_000 memory) in
        A_run_provers { dirs; provers; timeout; memory; stack; pattern; j; loc }
      );
      ( is_applied "run_provers_slurm",
        let* m = applied_fields "run_provers_slurm" in
        let* dirs = Fields.field m "dirs" atom_or_atom_list in
        let* provers = Fields.field m "provers" atom_or_atom_list in
        let* j = Fields.field_opt m "j" int in
        let* pattern = Fields.field_opt m "pattern" dec_regex in
        let* timeout = Fields.field_opt m "timeout" int in
        let* memory = Fields.field_opt m "memory" int in
        let* stack = Fields.field_opt m "stack" dec_stack_limit in
        let* partition = Fields.field_opt m "partition" string in
        let* nodes = Fields.field_opt m "nodes" int in
        let* addr_str_opt = Fields.field_opt m "addr" string in
        let* port = Fields.field_opt m "port" int in
        let* ntasks = Fields.field_opt m "ntasks" int in
        let+ () = Fields.check_no_field_left m in
        let memory = Some (CCOpt.get_or ~default:10_000_000 memory) in
        A_run_provers_slurm
          {
            dirs;
            provers;
            timeout;
            memory;
            stack;
            pattern;
            j;
            partition;
            nodes;
            addr = CCOpt.map Unix.inet_addr_of_string addr_str_opt;
            port;
            ntasks;
            loc;
          } );
      ( is_applied "progn",
        let+ l = applied "progn" self in
        A_progn l );
      ( is_applied "run_cmd",
        let+ cmd = applied1 "run_cmd" string in
        A_run_cmd { cmd; loc } );
      ( is_applied "git_checkout",
        let* m = applied_fields "git_checkout" in
        let* dir = Fields.field m "dir" string in
        let* ref = Fields.field m "ref" string in
        let* fetch_first = Fields.field_opt m "fetch_first" dec_fetch_first in
        let+ () = Fields.check_no_field_left m in
        A_git_checkout { dir; ref; fetch_first; loc } );
    ]

let dec (st : state) : t list SD.t =
  let open SD in
  let* loc = value_loc in
  try_l ~msg:"expected a stanza"
    [
      ( is_applied "dir",
        let* m = applied_fields "dir" in
        let* name = Fields.field_opt m "name" string in
        let* path = Fields.field m "path" string in
        let* expect = Fields.field_opt m "expect" (dec_expect st) in
        let* pattern = Fields.field_opt m "pattern" dec_regex in
        let+ () = Fields.check_no_field_left m in
        [ St_dir { name; path; expect; pattern; loc } ] );
      ( is_applied "import-prelude",
        let+ b = applied1 "import-prelude" bool in
        st.parse_prelude <- b;
        [] );
      ( is_applied "prover",
        let* m = applied_fields "prover" in
        let dec_tag_name =
          let* name = atom in
          if List.mem name st.tags then
            return name
          else
            failf (fun k ->
                k "unknown tag %S, declare it with `custom-tag`" name)
        in
        let dec_tags =
          list_of ~what:"pairs (tag regex)"
            (with_msg ~msg:"expected pair (tag regex)"
            @@ pair dec_tag_name dec_regex)
        in
        let* custom =
          let+ l = Fields.field_opt m "tags" dec_tags in
          CCOpt.get_or ~default:[] l
        in
        let* name = Fields.field m "name" string in
        let* cmd = Fields.field_opt m "cmd" string in
        let* version = Fields.field_opt m "version" dec_version in
        let* sat = Fields.field_opt m "sat" dec_regex in
        let* unsat = Fields.field_opt m "unsat" dec_regex in
        let* unknown = Fields.field_opt m "unknown" dec_regex in
        let* timeout = Fields.field_opt m "timeout" dec_regex in
        let* memory = Fields.field_opt m "memory" dec_regex in
        let* ulimits = Fields.field_opt m "ulimits" dec_ulimits in
        let* produces_proof = Fields.field_opt m "produces_proof" bool in
        let* proof_ext = Fields.field_opt m "proof_ext" string in
        let* proof_checker = Fields.field_opt m "proof_checker" string in
        let* inherits = Fields.field_opt m "inherits" string in
        let* () = Fields.check_no_field_left m in
        let* () =
          if cmd = None && inherits = None then
            failf (fun k -> k "one of 'cmd', 'inherits' must be set")
          else
            return ()
        in

        let st =
          St_prover
            {
              name;
              cmd;
              version;
              sat;
              unsat;
              unknown;
              timeout;
              memory;
              custom;
              ulimits;
              loc;
              produces_proof;
              proof_ext;
              inherits;
              proof_checker;
            }
        in

        return [ st ] );
      ( is_applied "proof_checker",
        let* m = applied_fields "proof_checker" in
        let* name = Fields.field m "name" string in
        let* cmd = Fields.field m "cmd" string in
        let* valid = Fields.field m "valid" dec_regex in
        let* invalid = Fields.field m "invalid" dec_regex in
        let+ () = Fields.check_no_field_left m in
        [ St_proof_checker { name; cmd; valid; invalid; loc } ] );
      ( is_applied "task",
        let* m = applied_fields "task" in
        let* name = Fields.field m "name" string in
        let* synopsis = Fields.field_opt m "synopsis" string in
        let* action = Fields.field m "action" dec_action in
        let+ () = Fields.check_no_field_left m in
        [ St_task { name; synopsis; action; loc } ] );
      ( is_applied "set-options",
        let* m = applied_fields "set-options" in
        let* progress = Fields.field_opt m "progress" bool in
        let* j = Fields.field_opt m "j" int in
        let+ () = Fields.check_no_field_left m in
        [ St_set_options { progress; j; loc } ] );
      ( is_applied "custom-tag",
        let* m = applied_fields "custom-tag" in
        let* s = Fields.field m "name" string in
        let+ () = Fields.check_no_field_left m in
        st.tags <- s :: st.tags;
        [ St_declare_custom_tag { tag = s; loc } ] );
    ]

let parse_string_list_ ~filename str : _ list =
  let module Se = Se.Sexp in
  let buf = Lexing.from_string ~with_positions:true str in
  Misc.set_lexbuf_filename buf filename;
  let d = Se.Decoder.of_lexbuf buf in
  let rec iter acc =
    match Se.Decoder.next d with
    | Se.End -> List.rev acc
    | Se.Yield x -> iter (x :: acc)
    | Se.Fail msg ->
      (* FIXME: get location from Sexp_loc iself? *)
      let loc = Loc.of_lexbuf ~input:(Loc.Input.string str) buf in
      let err = Error.make ~loc msg in
      Log.debug (fun k -> k "parse_string_list failed: %a" Error.pp err);
      Error.raise err
  in
  iter []

let _parse_prelude, parse_files, parse_string =
  let st = { parse_prelude = true; tags = [] } in

  let mk_error_stanza ~loc ~reify_errors (e : Error.t) =
    if reify_errors then (
      let st = St_error { err = e; loc } in
      st
    ) else
      Error.raise e
  in

  let decode_sexp_l ~reify_errors l =
    CCList.flat_map
      (fun s ->
        match Sexp_decode.run (dec st) s with
        | Ok x -> x
        | Error e ->
          let loc = Sexp_decode.Err.loc e in
          let err = SD.Err.to_error e in
          [ mk_error_stanza ~loc ~reify_errors err ])
      l
  in
  (* prelude? *)
  let parse_prelude ~reify_errors () =
    Error.guard (Error.wrap "Reading builtin config") @@ fun () ->
    let l =
      parse_string_list_ ~filename:"builtin_config.sexp" Builtin_config.config
    in
    let l = decode_sexp_l ~reify_errors l in
    St_enter_file "prelude" :: l
  in
  let process_file ~reify_errors file =
    let file = Misc.mk_abs_path file in
    match Se.parse_file_l file with
    | Error e -> Error.failf ?loc:None "cannot parse file '%s':@ %s" file e
    | Ok l ->
      let l = decode_sexp_l ~reify_errors l in
      St_enter_file file :: l
  and process_string ~reify_errors ~filename s =
    match Se.parse_string_l ~filename s with
    | Error e -> Error.failf ?loc:None "cannot parse file '%s':@ %s" filename e
    | Ok l ->
      let l = decode_sexp_l ~reify_errors l in
      St_enter_file filename :: l
  in
  let parse_files ?(reify_errors = false) (files : string list) : t list =
    let l = CCList.flat_map (process_file ~reify_errors) files in
    if st.parse_prelude then
      parse_prelude ~reify_errors () @ l
    else
      l
  and parse_string ?(reify_errors = false) ~filename (s : string) : t list =
    let l = process_string ~reify_errors ~filename s in
    if st.parse_prelude then
      parse_prelude ~reify_errors () @ l
    else
      l
  in
  parse_prelude, parse_files, parse_string

let prover_wl_to_st
    With_loc.
      {
        view =
          Prover.
            {
              name;
              cmd;
              sat;
              unsat;
              timeout;
              unknown;
              memory;
              ulimits;
              version;
              custom;
              inherits;
              produces_proof;
              proof_ext;
              proof_checker;
              _;
            };
        loc;
      } =
  St_prover
    {
      name;
      cmd = Some cmd;
      sat;
      unsat;
      timeout;
      unknown;
      memory;
      version = Some (Version_exact version);
      custom;
      ulimits = Some ulimits;
      loc;
      produces_proof = Some produces_proof;
      proof_ext;
      proof_checker;
      inherits;
    }

let proof_checker_wl_to_st
    With_loc.{ view = Proof_checker.{ name; cmd; valid; invalid }; loc } =
  St_proof_checker { name; cmd; valid; invalid; loc }

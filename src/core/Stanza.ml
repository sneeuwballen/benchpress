
(** {1 Configuration Stanzas} *)

open Common
module E = Or_error
module Se = Sexp_loc
module SD = Sexp_decode

type loc = Loc.t
type 'a or_error = 'a Or_error.t

module Log = (val Logs.src_log (Logs.Src.create "benchpress.stanzas"))

(** {2 Type Definitions} *)

(** Result to expect for a problem *)
type expect =
  | E_const of Res.t
  | E_program of { prover: string }
  | E_try of expect list (** Try these methods successively *)

type version_field =
  | Version_exact of Prover.version
  | Version_git of {dir:string} (* compute by calling git *)
  | Version_cmd of {cmd:string}

type stack_limit =
  | Unlimited
  | Limited of int

(** A regex in Perl syntax *)
type regex = string

type git_fetch = GF_fetch | GF_pull

type action =
  | A_run_provers of {
      dirs: string list; (* list of directories to examine *)
      pattern: regex option;
      provers: string list;
      timeout: int option;
      memory: int option;
      stack : stack_limit option;
      loc: Loc.t;
    }
  | A_git_checkout of {
      dir: string;
      ref: string;
      fetch_first: git_fetch option;
      loc: Loc.t;
    }
  | A_run_cmd of {
      cmd: string;
      loc: Loc.t;
    }
  | A_progn of action list

(** Stanzas for the configuration *)
type t =
  | St_enter_file of string
  | St_prover of {
      name: string;
      loc: Loc.t;
      version: version_field option;
      cmd: string;
      (** the command line to run.
          possibly contains $binary, $file, $memory and $timeout *)

      binary: string option; (** name of the program itself *)
      binary_deps: string list; (** list of binaries this depends on *)

      ulimits : Ulimit.conf option; (** which limits to enforce using ulimit *)

      (* Result analysis *)
      unsat   : regex option;  (** regex for "unsat" *)
      sat     : regex option;  (** regex for "sat" *)
      unknown : regex option;  (** regex for "unknown" *)
      timeout : regex option;  (** regex for "timeout" *)
      memory  : regex option;  (** regex for "out of memory" *)
      custom  : (string * regex) list; (** regex for custom results *)
    }
  | St_dir of {
      path: string;
      expect: expect option;
      pattern: regex option; (** Pattern of problems in this directory *)
      loc: Loc.t;
    }
  | St_task of {
      name: string; (* name of this task *)
      synopsis: string option;
      action: action;
      loc: Loc.t;
    }
  | St_set_options of {
      progress: bool option;
      j: int option;
      loc: Loc.t;
    }
  | St_declare_custom_tag of {
      tag: string;
      loc: Loc.t
    }
  | St_error of {
      err: Sexp_decode.err;
      loc: Loc.t;
    }

(** {2 Printers} *)

let rec pp_expect out = function
  | E_const r -> Fmt.fprintf out "(const %a)" Res.pp r
  | E_program {prover} -> Fmt.fprintf out "(run %s)" prover
  | E_try l -> Fmt.fprintf out "(@[try@ %a@])" (Misc.pp_list pp_expect) l

let pp_version_field out =
  let open Misc.Pp in
  function
  | Version_exact v ->  Prover.Version.pp out v
  | Version_git {dir} -> pp_str out @@ Printf.sprintf {|git:%S|} dir
  | Version_cmd {cmd} -> pp_str out @@ Printf.sprintf {|cmd:%S|} cmd

let pp_git_fetch out = function
  | GF_fetch -> Fmt.string out "fetch"
  | GF_pull -> Fmt.string out "pull"

let pp_stack_limit out = function
  | Unlimited -> Fmt.string out "unlimited"
  | Limited i -> Fmt.int out i

let rec pp_action out =
  let open Misc.Pp in
  function
  | A_run_provers {dirs;provers;timeout;memory;stack;pattern;loc=_} ->
    Fmt.fprintf out "(@[<v>run_provers%a%a%a%a%a%a@])"
      (pp_f "dirs" (pp_l pp_str)) dirs
      (pp_f "provers" (pp_l pp_str)) provers
      (pp_opt "pattern" pp_regex) pattern
      (pp_opt "timeout" Fmt.int) timeout
      (pp_opt "memory" Fmt.int) memory
      (pp_opt "stack" pp_stack_limit) stack
  | A_progn l -> Fmt.fprintf out "(@[progn %a@])" (pp_l pp_action) l
  | A_run_cmd {cmd=s;loc=_} -> Fmt.fprintf out "(@[run_cmd %a@])" pp_regex s
  | A_git_checkout {dir;ref;fetch_first;loc=_;} ->
    Fmt.fprintf out "(@[<v>git_checkout%a%a%a@])"
      (pp_f "dir" pp_regex) dir
      (pp_f "ref" pp_regex) ref
      (pp_opt "fetch-first" pp_git_fetch) fetch_first

let pp out =
  let open Misc.Pp in
  function
  | St_enter_file f -> Fmt.fprintf out "(@[enter-file@ %a@])" pp_str f
  | St_dir {path; expect; pattern; loc=_; } ->
    Fmt.fprintf out "(@[<v>dir%a%a%a@])"
      (pp_f "path" Fmt.string) path
      (pp_opt "expect" pp_expect) expect
      (pp_opt "pattern" pp_regex) pattern
  | St_prover {
      name; cmd; version; unsat; sat; unknown; timeout; memory;
      binary=_; binary_deps=_; custom; ulimits; loc=_;
    } ->
    let pp_custom out (x,y) =
      Fmt.fprintf out "(@[tag %a@ %a@])" pp_str x pp_regex y in
    Fmt.fprintf out "(@[<v>prover%a%a%a%a%a%a%a%a%a%a@])"
      (pp_f "name" pp_str) name
      (pp_f "cmd" pp_str) cmd
      (pp_opt "version" pp_version_field) version
      (pp_opt "ulimits" Ulimit.pp) ulimits
      (pp_opt "sat" pp_regex) sat
      (pp_opt "unsat" pp_regex) unsat
      (pp_opt "unknown" pp_regex) unknown
      (pp_opt "timeout" pp_regex) timeout
      (pp_opt "memory" pp_regex) memory
      (pp_l1 pp_custom) custom
  | St_task { name; synopsis; action; loc=_; } ->
    Fmt.fprintf out "(@[<v>task%a%a%a@])"
      (pp_f "name" pp_str) name
      (pp_opt "synopsis" pp_str) synopsis
      (pp_f "action" pp_action) action
  | St_set_options {j; progress;loc=_} ->
    Fmt.fprintf out "(@[<v>set-options%a%a])"
      (pp_opt "progress" Fmt.bool) progress
      (pp_opt "j" Fmt.int) j
  | St_declare_custom_tag {tag=t;loc=_} ->
    Fmt.fprintf out "(custom-tag %s)" t
  | St_error {err; loc=_} ->
    Fmt.fprintf out "(@[error %a@])" Error.pp (SD.Err.to_error err)

let pp_l out l =
  Fmt.fprintf out "@[<v>%a@]" (Misc.pp_list pp) l

(** {2 Decoding} *)

let dec_res tags =
  let open SD in
  let* s = atom in
  (try return (Res.of_string ~tags s)
   with _ -> failf (fun k->k"expected a `Res.t`, not %S" s))

let dec_regex : regex SD.t =
  let valid_re s =
    try ignore (Re.Perl.compile_pat s); true
    with _ -> false
  in
  let open SD in
  let* s = atom in
  if valid_re s then return s else fail "expected a valid Perl regex"

let dec_expect tags : _ SD.t =
  let open SD in
  fix @@ fun self ->
  try_l ~msg:"expect stanzas tag (constructors: const|run|try)"
    [ is_applied "const", (let+ r = applied1 "const" (dec_res tags) in E_const r);
      is_applied "run", (let+ prover = applied1 "run" atom in E_program {prover});
      is_applied "try", (let+ e = applied "try" self in E_try e);
    ]

let dec_version : _ SD.t =
  let open SD in
  let str =
    atom >>= fun s ->
    return @@ if CCString.prefix ~pre:"git:" s then (
      Version_git {dir=snd @@ CCString.Split.left_exn ~by:":" s}
    ) else if CCString.prefix ~pre:"cmd:" s then (
      Version_cmd {cmd=snd @@ CCString.Split.left_exn ~by:":" s}
    ) else (
      Version_exact (Prover.Tag s)
    )
  in
  try_l ~msg:"expected version" [
    (is_atom, str);
    (is_applied "git",
     let* m = applied_fields "git" in
     let* branch = Fields.field m "branch" atom in
     let* commit = Fields.field m "commit" atom in
     let+ () = Fields.check_no_field_left m in
     Version_exact (Prover.Git {branch; commit})
    );
  ]

let dec_ulimits : _ SD.t =
  let open SD in
  let no_limits = Ulimit.mk ~time:false ~memory:false ~stack:false in
  let none =
    let* s = atom in
    if s="none" then return no_limits
    else fail {|expected "none"|}
  in
  let single_limit acc (s:string) =
    match s with
    | "time" -> return { acc with Ulimit.time = true; }
    | "memory" -> return { acc with Ulimit.memory = true; }
    | "stack" -> return { acc with Ulimit.stack = true; }
    | s -> failf (fun k->k"expected 'ulimit' stanzas (constructors: time|memory|stack, not %S)" s)
  in
  try_l ~msg:"expected ulimit" [
    is_atom, none;
    (is_list,
     let* l = list_of atom in
     fold_l single_limit no_limits l);
  ]

let dec_fetch_first =
  let open SD in
  keyword ~msg:"expected fetch_first"
    ["fetch", GF_fetch;
     "pull", GF_pull;
    ]

let dec_stack_limit : _ SD.t =
  let open SD in
  try_l ~msg:{|expected stack_limit ("unlimited" or an integer)|} [
    (try_succeed @@ let+ x = int in (Limited x));
    (is_atom, keyword ~msg:"unlimited" ["unlimited", Unlimited]);
  ]

let dec_action : action SD.t =
  let open SD in
  fix @@ fun self ->
  let* loc = value_loc in
  try_l ~msg:"expected an action" [
    (is_applied "run_provers",
     let* m = applied_fields "run_provers" in
     let* dirs = Fields.field m "dirs" atom_or_atom_list in
     let* provers = Fields.field m "provers" atom_or_atom_list in
     let* pattern = Fields.field_opt m "pattern" dec_regex in
     let* timeout = Fields.field_opt m "timeout" int in
     let* memory = Fields.field_opt m "memory" int in
     let* stack = Fields.field_opt m "stack" dec_stack_limit in
     let+ () = Fields.check_no_field_left m in
     let memory = Some (CCOpt.get_or ~default:10_000_000 memory) in
     A_run_provers {dirs;provers;timeout;memory;stack;pattern;loc}
    );
    (is_applied "progn",
     let+ l = applied "progn" self in
     A_progn l
    );
    (is_applied "run_cmd",
     let+ cmd = applied1 "run_cmd" string in
     A_run_cmd {cmd; loc}
    );
    (is_applied "git_checkout",
     let* m = applied_fields "git_checkout" in
     let* dir = Fields.field m "dir" string in
     let* ref = Fields.field m "ref" string in
     let* fetch_first = Fields.field_opt m "fetch_first" dec_fetch_first in
     let+ () = Fields.check_no_field_left m in
     A_git_checkout {dir; ref; fetch_first;loc}
    );
  ]

(* TODO: carry definitions around? *)
let dec tags : (_ list * t) SD.t =
  let open SD in
  let* loc = value_loc in
  try_l ~msg:"expected a stanza" [
    (is_applied "dir",
     let* m = applied_fields "dir" in
     let* path = Fields.field m "path" string in
     let* expect = Fields.field_opt m "expect" (dec_expect tags) in
     let* pattern = Fields.field_opt m "pattern" dec_regex in
     let+ () = Fields.check_no_field_left m in
     (tags, St_dir {path;expect;pattern;loc})
    );
    (is_applied "prover",
     let* m = applied_fields "prover" in
     let dec_tag_name =
       let* name = atom in
       if List.mem name tags then return name
       else failf (fun k->k"unknown tag %S, declare it with `cutom-tag`" name)
     in
     let dec_tags =
       list_of (pair dec_tag_name dec_regex)
     in
     let* custom =
       let+ l = Fields.field_opt m "tags" dec_tags in
       CCOpt.get_or ~default:[] l
     in
     let* name = Fields.field m "name" string in
     let* cmd = Fields.field m "cmd" string in
     let* version = Fields.field_opt m "version" dec_version in
     let* sat = Fields.field_opt m "sat" dec_regex in
     let* unsat = Fields.field_opt m "unsat" dec_regex in
     let* unknown = Fields.field_opt m "unknown" dec_regex in
     let* timeout = Fields.field_opt m "timeout" dec_regex in
     let* memory = Fields.field_opt m "memory" dec_regex in
     let* ulimits = Fields.field_opt m "ulimit" dec_ulimits in
     let+ () = Fields.check_no_field_left m in
     (tags, St_prover {
         name; cmd; version; sat; unsat; unknown; timeout; memory; custom;
         ulimits; loc;
         binary=None;
         binary_deps=[]; (* TODO *)
       })
    );
    (is_applied "task",
     let* m = applied_fields "task" in
     let* name = Fields.field m "name" string in
     let* synopsis = Fields.field_opt m "synopsis" string in
     let* action = Fields.field m "action" dec_action in
     let+ () = Fields.check_no_field_left m in
     (tags, St_task {name;synopsis;action;loc})
    );
    (is_applied "set-options",
     let* m = applied_fields "set-options" in
      let* progress = Fields.field_opt m "progress" bool in
      let* j = Fields.field_opt m "j" int in
      let+ () = Fields.check_no_field_left m in
      (tags, St_set_options {progress; j; loc})
    );
    (is_applied "custom-tag",
     let* m = applied_fields "custom-tag" in
     let* s = Fields.field m "name" string in
     let+ () = Fields.check_no_field_left m in
     (s::tags,St_declare_custom_tag {tag=s;loc})
    );
  ]

exception Wrap of Error.t
let fail_with_error e =  raise (Wrap e)
let fail_with_error_f ?loc fmt = Format.kasprintf (fun s ->raise (Wrap (Error.make ?loc s))) fmt

let parse_string_list_ ~filename str : _ list or_error =
  let module Se = Se.Sexp in
  let buf = Lexing.from_string ~with_positions:true str in
  Lexing.set_filename buf filename;
  let d = Se.Decoder.of_lexbuf buf in
  let rec iter acc = match Se.Decoder.next d with
    | Se.End -> Result.Ok (List.rev acc)
    | Se.Yield x -> iter (x::acc)
    | Se.Fail msg ->
      (* FIXME: get location from Sexp_loc iself? *)
      let loc = Loc.of_lexbuf ~input:(Loc.Input.string str) buf in
      let err = Error.make ~loc msg in
      Log.debug (fun k->k"parse_string_list failed: %a" Error.pp err);
      Result.Error err
  in
  try iter []
  with e ->
    CCResult.of_exn e
    |> CCResult.map_err Error.make

(** Parse a list of files into a list of stanzas *)
let parse_files, parse_string =
  let decode_sexp_l ~reify_errors l =
    CCList.fold_map
      (fun tags s ->
         match Sexp_decode.run (dec tags) s with
         | Ok x -> x
         | Error e ->
           let loc = Sexp_decode.Err.loc e in
           if reify_errors then (
             let st = St_error {err=e; loc} in
             tags, st
           ) else (
             raise (Wrap (Sexp_decode.Err.to_error e))
           ))
      l
  in
  (* prelude? *)
  let get_prelude ~reify_errors ~builtin () =
    let tags, prelude =
      if builtin then
        match parse_string_list_ ~filename:"builtin_config.sexp" Builtin_config.config with
        | Ok l ->
          let tags, l = decode_sexp_l ~reify_errors [] l in
          tags, St_enter_file "prelude" :: l
        | Error e ->
          fail_with_error @@ Error.wrap "Reading builtin config" e
      else [], []
    in
    tags, prelude
  in
  let process_file ~reify_errors tags file =
    let file = Misc.mk_abs_path file in
    match Se.parse_file_l file with
    | Error e -> fail_with_error_f ?loc:None "cannot parse file '%s':@ %s" file e
    | Ok l ->
      let tags, l = decode_sexp_l ~reify_errors tags l in
      tags, St_enter_file file :: l
  and process_string ~reify_errors ~filename tags s =
    match Se.parse_string_l ~filename s with
    | Error e -> fail_with_error_f ?loc:None "cannot parse file '%s':@ %s" filename e
    | Ok l ->
      let tags, l = decode_sexp_l ~reify_errors tags l in
      tags, St_enter_file filename :: l
  in
  let wrap_err_ f =
    try f ()
    with Wrap e -> Error e
  in
  let parse_files ?(reify_errors=false) ?(builtin=true) (files:string list) : t list or_error =
    wrap_err_ @@ fun () ->
    let tags, prelude = get_prelude ~reify_errors ~builtin () in
    CCList.fold_map (process_file ~reify_errors) tags files
    |> snd
    |> CCList.cons prelude
    |> CCList.flatten
    |> E.return
  and parse_string ?(reify_errors=false) ?(builtin=true) ~filename (s:string) : t list or_error =
    wrap_err_ @@ fun () ->
    let tags, prelude = get_prelude ~reify_errors ~builtin () in
    let _tags, l = process_string ~reify_errors ~filename tags s in
    CCList.cons prelude [l]
    |> CCList.flatten
    |> E.return
  in
  parse_files, parse_string


(** {1 Configuration Stanzas} *)

module E = CCResult
module Fmt = CCFormat
module Se = Sexp_loc

type 'a or_error = ('a, string) E.t

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
    }
  | A_git_checkout of {
      dir: string;
      ref: string;
      fetch_first: git_fetch option;
    }
  | A_run_cmd of string
  | A_progn of action list

(** Stanzas for the configuration *)
type t =
  | St_enter_file of string
  | St_prover of {
      name: string;
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
    }
  | St_task of {
      name: string; (* name of this task *)
      synopsis: string option;
      action: action;
    }
  | St_set_options of {
      progress: bool option;
      j: int option;
    }
  | St_declare_custom_tag of string

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
  | A_run_provers {dirs;provers;timeout;memory;stack;pattern;} ->
    Fmt.fprintf out "(@[<v>run_provers%a%a%a%a%a%a@])"
      (pp_f "dirs" (pp_l pp_str)) dirs
      (pp_f "provers" (pp_l pp_str)) provers
      (pp_opt "pattern" pp_regex) pattern
      (pp_opt "timeout" Fmt.int) timeout
      (pp_opt "memory" Fmt.int) memory
      (pp_opt "stack" pp_stack_limit) stack
  | A_progn l -> Fmt.fprintf out "(@[progn %a@])" (pp_l pp_action) l
  | A_run_cmd s -> Fmt.fprintf out "(@[run_cmd %a@])" pp_regex s
  | A_git_checkout {dir;ref;fetch_first} ->
    Fmt.fprintf out "(@[<v>git_checkout%a%a%a@])"
      (pp_f "dir" pp_regex) dir
      (pp_f "ref" pp_regex) ref
      (pp_opt "fetch-first" pp_git_fetch) fetch_first

let pp out =
  let open Misc.Pp in
  function
  | St_enter_file f -> Fmt.fprintf out "(@[enter-file@ %a@])" pp_str f
  | St_dir {path; expect; pattern; } ->
    Fmt.fprintf out "(@[<v>dir%a%a%a@])"
      (pp_f "path" Fmt.string) path
      (pp_opt "expect" pp_expect) expect
      (pp_opt "pattern" pp_regex) pattern
  | St_prover {
      name; cmd; version; unsat; sat; unknown; timeout; memory;
      binary=_; binary_deps=_; custom; ulimits;
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
  | St_task {name; synopsis; action;} ->
    Fmt.fprintf out "(@[<v>task%a%a%a@])"
      (pp_f "name" pp_str) name
      (pp_opt "synopsis" pp_str) synopsis
      (pp_f "action" pp_action) action
  | St_set_options {j; progress} ->
    Fmt.fprintf out "(@[<v>set-options%a%a])"
      (pp_opt "progress" Fmt.bool) progress
      (pp_opt "j" Fmt.int) j
  | St_declare_custom_tag t ->
    Fmt.fprintf out "(custom-tag %s)" t

let pp_l out l =
  Fmt.fprintf out "@[<v>%a@]" (Misc.pp_list pp) l

(** {2 Decoding} *)

let fail_f fmt = Format.kasprintf (fun s -> Se.D.fail s) fmt
let fail_sexp_f fmt =
  Format.kasprintf
    (fun s ->
       let open Se.D in
       value >>= fun sexp ->
       fail @@ Format.asprintf "@[<v>at %a:@,%s@]" Se.pp_loc sexp.Se.loc s)
    fmt

let dec_res tags =
  let open Se.D in
  string >>= fun s ->
  (try succeed (Res.of_string ~tags s)
   with _ -> fail_sexp_f "expected a `Res.t`, not %S" s)

let dec_regex : regex Se.D.decoder =
  let valid_re s =
    try ignore (Re.Perl.compile_pat s); true
    with _ -> false
  in
  let open Se.D in
  string >>= fun s ->
  if valid_re s then succeed s else fail "expected a valid Perl regex"

let dec_expect tags : _ Se.D.decoder =
  let open Se.D in
  fix (fun self ->
      string >>:: function
      | "const" -> list1 (dec_res tags) >|= fun r -> E_const r
      | "run" -> list1 string >|= fun prover -> E_program {prover}
      | "try" -> list self >|= fun e -> E_try e
      | s -> fail_sexp_f "expected `expect` stanzas (constructors: const|run|try, not %S)" s)

let dec_version : _ Se.D.decoder =
  let open Se.D in
  let str =
    string >>= fun s ->
    succeed @@ if CCString.prefix ~pre:"git:" s then (
      Version_git {dir=snd @@ CCString.Split.left_exn ~by:":" s}
    ) else if CCString.prefix ~pre:"cmd:" s then (
      Version_cmd {cmd=snd @@ CCString.Split.left_exn ~by:":" s}
    ) else (
      Version_exact (Prover.Tag s)
    )
  in
  one_of [
    "atom", str;
    "list", (string >>:: function
      | "git" ->
        field "branch" string >>= fun branch ->
        field "commit" string >>= fun commit ->
        succeed (Version_exact (Prover.Git {branch; commit}))
      | s -> fail_sexp_f "invalid `version` constructor: %s" s);
  ]

let dec_ulimits : _ Se.D.decoder =
  let open Se.D in
  let no_limits = Ulimit.mk ~time:false ~memory:false ~stack:false in
  let none =
    string >>= function
    | "none" -> succeed no_limits
    | _ -> fail_sexp_f {|expected "none"|}
  in
  let single_limit acc =
    string >>= function
    | "time" -> succeed { acc with Ulimit.time = true; }
    | "memory" -> succeed { acc with Ulimit.memory = true; }
    | "stack" -> succeed { acc with Ulimit.stack = true; }
    | s -> fail_sexp_f "expected 'ulimit' stanzas (constructors: time|memory|stack, not %S)" s
  in
  one_of [
    "atom", none;
    "list", list_fold_left single_limit no_limits;
  ]

let list_or_singleton d =
  let open Se.D in
  value >>= fun s ->
  (* turn atoms into lists *)
  let l = match s.Se.view with
    | Atom _ -> Se.of_list [s]
    | List _ -> s
  in
  from_result (decode_value (list d) l)

let dec_fetch_first =
  let open Se.D in
  string >>= function
  | "fetch" -> succeed GF_fetch
  | "pull" -> succeed GF_pull
  | _ -> fail_f "expected `fetch` or `pull`"

let dec_stack_limit : _ Se.D.decoder =
  let open Se.D in
  one_of [
    "int", int >>= (fun s -> succeed (Limited s));
    "unlimited", string >>= function
      | "unlimited" -> succeed Unlimited
      | _ -> fail_sexp_f "expect 'unlimited' of an integer"
  ]

let dec_action : action Se.D.decoder =
  let open Se.D in
  fix (fun self ->
      string >>:: function
      | "run_provers" ->
        field "dirs" (list_or_singleton string) >>= fun dirs ->
        field "provers" (list_or_singleton string) >>= fun provers ->
        field_opt "pattern" dec_regex >>= fun pattern ->
        field_opt "timeout" int >>= fun timeout ->
        field_opt "memory" int >>= fun memory ->
        field_opt "stack" dec_stack_limit >>= fun stack ->
        succeed @@ A_run_provers {dirs;provers;timeout;memory;stack;pattern}
      | "progn" -> list_or_singleton self >|= fun l -> A_progn l
      | "run_cmd" -> list1 string >|= fun s -> A_run_cmd s
      | "git_checkout" ->
        field "dir" string >>= fun dir ->
        field "ref" string >>= fun ref ->
        field_opt "fetch_first" dec_fetch_first >>= fun fetch_first ->
        succeed @@ A_git_checkout {dir; ref; fetch_first}
      | s ->
        fail_sexp_f "unknown config stanzas %s" s)

(* TODO: carry definitions around? *)
let dec tags : (_ list * t) Se.D.decoder =
  let open Se.D in
  string >>:: function
  | "dir" ->
    field "path" string >>= fun path ->
    field_opt "expect" (dec_expect tags) >>= fun expect ->
    field_opt "pattern" dec_regex >>= fun pattern ->
    succeed (tags, St_dir {path;expect;pattern})
  | "prover" ->
    let tag = string >>:: function
      | "tag" ->
        string >>:: fun name ->
        if List.mem name tags then (
          dec_regex >>:: fun re -> succeed @@ Some (name,re)
        ) else (
          fail_f "tag '%s' was not declared, use a `custom-tag` stanza" name
        )
      | _ -> succeed None
    in
    field "name" string >>= fun name ->
    field "cmd" string >>= fun cmd ->
    field_opt "version" dec_version >>= fun version ->
    field_opt "sat" dec_regex >>= fun sat ->
    field_opt "unsat" dec_regex >>= fun unsat ->
    field_opt "unknown" dec_regex >>= fun unknown ->
    field_opt "timeout" dec_regex >>= fun timeout ->
    field_opt "memory" dec_regex >>= fun memory ->
    field_opt "ulimit" dec_ulimits >>= fun ulimits ->
    list_filter tag >>= fun custom ->
    succeed @@
    (tags, St_prover {
        name; cmd; version; sat; unsat; unknown; timeout; memory; custom;
        ulimits;
      binary=None;
      binary_deps=[]; (* TODO *)
    })
  | "task" ->
    field "name" string >>= fun name ->
    field_opt "synopsis" string >>= fun synopsis ->
    field "action" dec_action >>= fun action ->
    succeed @@ (tags, St_task {name;synopsis;action})
  | "set-options" ->
    field_opt "progress" bool >>= fun progress ->
    field_opt "j" int >>= fun j ->
    succeed @@ (tags, St_set_options {progress; j})
  | "custom-tag" ->
    field "name" string >>= fun s ->
    succeed @@ (s::tags,St_declare_custom_tag s)
  | s ->
    fail_sexp_f "unknown config stanzas %s" s

exception Wrap of string
let wrapf fmt = Format.kasprintf (fun s ->raise (Wrap s)) fmt

let parse_string_list_ s : _ list or_error =
  let buf = Lexing.from_string s in
  let d = Se.Decoder.of_lexbuf buf in
  let rec iter acc = match Se.Decoder.next d with
    | Se.End -> Result.Ok (List.rev acc)
    | Se.Yield x -> iter (x::acc)
    | Se.Fail e -> Result.Error e
  in
  try iter []
  with e -> E.of_exn e

(** Parse a list of files into a list of stanzas *)
let parse_files ?(builtin=true) (files:string list) : t list or_error =
  let decode_sexp_l l =
    CCList.fold_map
      (fun tags s ->
         match Se.D.decode_value (dec tags) s with
         | Ok x -> x
         | Error e ->
           wrapf "at %a, error@ %s" Se.pp_loc s.Se.loc
             (Se.D.string_of_error e))
      l
  in
  try
    let tags, prelude =
      if builtin then
        match parse_string_list_ Builtin_config.config with
        | Ok l ->
          let tags, l = decode_sexp_l [] l in
          tags, St_enter_file "prelude" :: l
        | Error e ->
          wrapf "failure when reading builtin config: %s" e
      else [], []
    in
    CCList.fold_map
      (fun tags file ->
         Se.cur_file_ := file; (* issue in CCSexp's locations *)
         let file = Misc.mk_abs_path file in
         match Se.parse_file_list file with
         | Error e -> wrapf "cannot parse %s:@,%s" file e
         | Ok l ->
           let tags, l = decode_sexp_l tags l in
           tags, St_enter_file file :: l)
      tags files
    |> snd
    |> CCList.cons prelude
    |> CCList.flatten
    |> E.return
  with Wrap e -> Error e

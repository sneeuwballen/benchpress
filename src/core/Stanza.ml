
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

(** A regex in Perl syntax *)
type regex = string

type action =
  | A_run_provers of {
    dirs: string list; (* list of directories to examine *)
    provers: string list;
    timeout: int option;
    memory: int option;
  }

(** Stanzas for configuring Logitest *)
type t =
  | St_prover of {
      name: string;
      version: version_field option;
      cmd: string;
      (** the command line to run.
         possibly contains $binary, $file, $memory and $timeout *)

      binary: string option; (** name of the program itself *)
      binary_deps: string list; (** list of binaries this depends on *)

      (* Result analysis *)
      unsat   : regex option;  (** regex for "unsat" *)
      sat     : regex option;  (** regex for "sat" *)
      unknown : regex option;  (** regex for "unknown" *)
      timeout : regex option;  (** regex for "timeout" *)
      memory  : regex option;  (** regex for "out of memory" *)
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

(** {2 Printers} *)

let rec pp_expect out = function
  | E_const r -> Fmt.fprintf out "(const %a)" Res.pp r
  | E_program {prover} -> Fmt.fprintf out "(run %s)" prover
  | E_try l -> Fmt.fprintf out "(@[try@ %a@])" (Misc.pp_list pp_expect) l

let pp_version_field out =
  let open Misc.Pp in
  function
  | Version_exact v ->  Prover.pp_version out v
  | Version_git {dir} -> pp_str out @@ Printf.sprintf {|git:%S|} dir
  | Version_cmd {cmd} -> pp_str out @@ Printf.sprintf {|cmd:%S|} cmd

let pp_action out =
  let open Misc.Pp in
  function
  | A_run_provers {dirs;provers;timeout;memory} ->
    Fmt.fprintf out "(@[<v1>run_provers%a%a%a%a@])"
      (pp_f "dirs" (pp_l pp_str)) dirs
      (pp_f "provers" (pp_l pp_str)) provers
      (pp_opt "timeout" Fmt.int) timeout
      (pp_opt "memory" Fmt.int) memory

let pp out =
  let open Misc.Pp in
  function
  | St_dir {path; expect; pattern; } ->
    Fmt.fprintf out "(@[<v1>dir%a%a%a@])"
      (pp_f "path" Fmt.string) path
      (pp_opt "expect" pp_expect) expect
      (pp_opt "pattern" pp_regex) pattern
  | St_prover {
      name; cmd; version; unsat; sat; unknown; timeout; memory;
      binary=_; binary_deps=_;
    } ->
    Fmt.fprintf out "(@[<v1>prover%a%a%a%a%a%a%a%a@])"
      (pp_f "name" pp_str) name
      (pp_f "cmd" pp_str) cmd
      (pp_opt "version" pp_version_field) version
      (pp_opt "sat" pp_regex) sat
      (pp_opt "unsat" pp_regex) unsat
      (pp_opt "unknown" pp_regex) unknown
      (pp_opt "timeout" pp_regex) timeout
      (pp_opt "memory" pp_regex) memory
  | St_task {name; synopsis; action;} ->
    Fmt.fprintf out "(@[<v1>task%a%a%a@])"
      (pp_f "name" pp_str) name
      (pp_opt "synopsis" pp_str) synopsis
      (pp_f "action" pp_action) action

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

let dec_res =
  let open Se.D in
  string >>= fun s ->
  (try succeed (Res.of_string s)
   with _ -> fail_sexp_f "expected a `Res.t`, not %S" s)

let dec_regex : regex Se.D.decoder =
  let valid_re s =
    try ignore (Re.Perl.compile_pat s); true
    with _ -> false
  in
  let open Se.D in
  string >>= fun s ->
  if valid_re s then succeed s else fail "expected a valid Perl regex"

let dec_expect : _ Se.D.decoder =
  let open Se.D in
  fix (fun self ->
      string >>:: function
      | "const" -> list1 dec_res >|= fun r -> E_const r
      | "run" -> list1 string >|= fun prover -> E_program {prover}
      | "try" -> list self >|= fun e -> E_try e
      | s -> fail_sexp_f "invalid `expect` stanzas: %s" s)

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

let list_or_singleton d =
  let open Se.D in
  value >>= fun s ->
  (* turn atoms into lists *)
  let l = match s.Se.view with
    | Atom _ -> Se.of_list [s]
    | List _ -> s
  in
  from_result (decode_value (list d) l)

let dec_action : action Se.D.decoder =
  let open Se.D in
  string >>:: function
  | "run_provers" ->
    field "dirs" (list_or_singleton string) >>= fun dirs ->
    field "provers" (list_or_singleton string) >>= fun provers ->
    field_opt "timeout" int >>= fun timeout ->
    field_opt "memory" int >>= fun memory ->
    succeed @@ A_run_provers {dirs;provers;timeout;memory}
  | s ->
    fail_sexp_f "unknown config stanzas %s" s
  
let dec : t Se.D.decoder =
  let open Se.D in
  string >>:: function
  | "dir" ->
    field "path" string >>= fun path ->
    field_opt "expect" dec_expect >>= fun expect ->
    field_opt "pattern" dec_regex >>= fun pattern ->
    succeed (St_dir {path;expect;pattern})
  | "prover" ->
    field "name" string >>= fun name ->
    field "cmd" string >>= fun cmd ->
    field_opt "version" dec_version >>= fun version ->
    field_opt "sat" dec_regex >>= fun sat ->
    field_opt "unsat" dec_regex >>= fun unsat ->
    field_opt "unknown" dec_regex >>= fun unknown ->
    field_opt "timeout" dec_regex >>= fun timeout ->
    field_opt "memory" dec_regex >>= fun memory ->
    succeed @@
    St_prover {
      name; cmd; version; sat; unsat; unknown; timeout; memory;
      binary=None; binary_deps=[]; (* TODO *)
    }
  | "task" ->
    field "name" string >>= fun name ->
    field_opt "synopsis" string >>= fun synopsis ->
    field "action" dec_action >>= fun action ->
    succeed @@ St_task {name;synopsis;action}
  | s ->
    fail_sexp_f "unknown config stanzas %s" s

exception Wrap of string
let wrapf fmt = Format.kasprintf (fun s ->raise (Wrap s)) fmt

(** Parse a list of files into a list of stanzas *)
let parse_files (files:string list) : t list or_error =
  try
    List.map
      (fun file ->
         Se.cur_file_ := file; (* issue in CCSexp *)
         match Se.parse_file_list file with
         | Error e -> wrapf "cannot parse %s:@,%s" file e
         | Ok l ->
           CCList.map
             (fun s ->
               match Se.D.decode_value dec s with
               | Ok x -> x
               | Error e ->
                 wrapf "at %a, error@ %s" Se.pp_loc s.Se.loc
                   (Se.D.string_of_error e))
             l)
      files
    |> CCList.flatten
    |> E.return
  with Wrap e -> Error e

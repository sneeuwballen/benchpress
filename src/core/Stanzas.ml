
(** {1 Configuration Stanzas} *)

module E = CCResult
module Fmt = CCFormat
module Se = Sexp_loc

type 'a or_error = ('a, string) E.t

(** {2 Type Definitions} *)

(** Result to expect for a problem *)
type expect =
  | E_auto
  | E_const of Res.t
  | E_program of { prover: string }
  | E_try of expect list (** Try these methods successively *)

(** A regex in Perl syntax *)
type regex = string

(** Stanzas for configuring Logitest *)
type t =
  | Add_prover of {
      name: string;
      version: Prover.version option;
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
  | Add_dir of {
      path: string;
      expect: expect option;
      pattern: regex option; (** Pattern of problems in this directory *)
    }
  | Add_task of {
      dirs: string list; (* list of directories to examine *)
      provers: string list;
      timeout: int option;
      memory: int option;
    }

(** {2 Printers} *)

let pp_l = Misc.pp_list
let pp_f_ what f out x = Fmt.fprintf out "@ (@[%s@ %a@])" what f x
let pp_opt_ what f out = function
  | None -> ()
  | Some x -> Fmt.fprintf out "@ (@[%s@ %a@])" what f x
let pp_str out s = Sexp_loc.pp out (Sexp_loc.atom s)
let pp_regex out r = Fmt.fprintf out "(re %a)" pp_str r

let rec pp_expect out = function
  | E_auto -> Fmt.string out "auto"
  | E_const r -> Fmt.fprintf out "(const %a)" Res.pp r
  | E_program {prover} -> Fmt.fprintf out "(run %s)" prover
  | E_try l -> Fmt.fprintf out "(@[try@ %a@])" (pp_l pp_expect) l

let pp out = function
  | Add_dir {path; expect; pattern; } ->
    Fmt.fprintf out "(@[<hv1>dir%a%a%a@])"
      (pp_f_ "path" Fmt.string) path
      (pp_opt_ "expect" pp_expect) expect
      (pp_opt_ "pattern" pp_regex) pattern
  | Add_prover {
      name; cmd; version; unsat; sat; unknown; timeout; memory;
      binary=_; binary_deps=_;
    } ->
    Fmt.fprintf out "(@[<hv1>prover%a%a%a%a%a%a%a%a@])"
      (pp_f_ "name" pp_str) name
      (pp_f_ "cmd" pp_str) cmd
      (pp_opt_ "version" Prover.pp_version) version
      (pp_opt_ "sat" pp_regex) sat
      (pp_opt_ "unsat" pp_regex) unsat
      (pp_opt_ "unknown" pp_regex) unknown
      (pp_opt_ "timeout" pp_regex) timeout
      (pp_opt_ "memory" pp_regex) memory
  | Add_task {dirs; provers; timeout; memory; } ->
    Fmt.fprintf out "(@[<hv1>task%a%a%a%a@])"
      (pp_f_ "dirs" (pp_l pp_str)) dirs
      (pp_f_ "provers" (pp_l pp_str)) provers
      (pp_opt_ "timeout" Fmt.int) timeout
      (pp_opt_ "memory" Fmt.int) memory

(** {2 Decoding} *)

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

let dec_expect : _ Se.D.decoder =
  let open Se.D in
  fix (fun self ->
    one_of [
      ("atomic `expect`", string >>= function
       | "auto" -> succeed E_auto
       | _ -> fail_sexp_f "expected `auto`");
      ("composite `expect`", string >>:: function
       | "auto" -> list0 >|= fun () -> E_auto
       | "const" -> list1 dec_res >|= fun r -> E_const r
       | "run" -> list1 string >|= fun prover -> E_program {prover}
       | "try" -> list self >|= fun e -> E_try e
       | s -> fail_sexp_f "invalid `expect` stanzas: %s" s)
    ])

let dec_version : _ Se.D.decoder =
  let open Se.D in
  one_of [
    "atom", (string >|= fun s -> Prover.Tag s);
    "list", (string >>:: function
      | "git" ->
        field "branch" string >>= fun branch ->
        field "commit" string >>= fun commit ->
        succeed (Prover.Git {branch; commit})
      | s -> fail_sexp_f "invalid `version` constructor: %s" s)
  ]
  
let dec : t Se.D.decoder =
  let open Se.D in
  string >>:: function
  | "dir" ->
    field "path" string >>= fun path ->
    field_opt "expect" dec_expect >>= fun expect ->
    field_opt "pattern" string >>= fun pattern ->
    succeed (Add_dir {path;expect;pattern})
  | "prover" ->
    field "name" string >>= fun name ->
    field "cmd" string >>= fun cmd ->
    field_opt "version" dec_version >>= fun version ->
    field_opt "sat" string >>= fun sat ->
    field_opt "unsat" string >>= fun unsat ->
    field_opt "unknown" string >>= fun unknown ->
    field_opt "timeout" string >>= fun timeout ->
    field_opt "memory" string >>= fun memory ->
    succeed @@
    Add_prover {
      name; cmd; version; sat; unsat; unknown; timeout; memory;
      binary=None; binary_deps=[]; (* TODO *)
    }
  | "task" ->
    assert false
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

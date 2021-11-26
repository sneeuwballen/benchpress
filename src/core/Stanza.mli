
(** Stanzas for the configuration language *)

open Common
module Se = Sexp_loc

type loc = Loc.t
type 'a or_error = 'a Or_error.t

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

val pp_expect : expect Fmt.printer
val pp_version_field: version_field Fmt.printer
val pp_git_fetch : git_fetch Fmt.printer
val pp_stack_limit : stack_limit Fmt.printer
val pp_action : action Fmt.printer
val pp : t Fmt.printer
val pp_l : t list Fmt.printer

(** {2 Decoding} *)

val parse_files :
  ?reify_errors:bool -> ?builtin:bool ->
  string list -> t list or_error
(** Parse a list of files and return their concatenated stanzas.
    @param builtin if true, add the builtin prelude before the files
    @param reify_errors if true, parsing errors become {!St_error}
*)

val parse_string :
  ?reify_errors:bool -> ?builtin:bool ->
  filename:string -> string -> t list or_error
(** Parse a string. See {!parse_files} for the arguments.
    @param filename name used in locations *)

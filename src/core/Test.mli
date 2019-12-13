(* This file is free software. See file "license" for more details. *)

(* TODO: rename this, split into several files *)
(** {1 Tools to test a prover} *)

type 'a or_error = ('a, string) CCResult.t

module Db = Sqlite3_utils
module MStr = Misc.Str_map
module J = Misc.Json

type result = Prover.name Run_result.t

module Raw : sig
  type t = result MStr.t

  val empty: t

  val add : result -> t -> t

  val merge : t -> t -> t

  type stat = {
    unsat: int;
    sat: int;
    errors: int;
    unknown: int;
    timeout: int;
    total_time: float; (* for sat+unsat *)
  }

  val stat : t -> stat

  val pp_stat : stat CCFormat.printer
  val printbox_stat : stat -> PrintBox.t

  val decode_stat : stat J.Decode.t

  val decode : t J.Decode.t
end

(** {2 Result on a single problem} *)

module Analyze : sig
  type t = {
    raw: Raw.t;
    stat: Raw.stat;
    improved  : result list;
    ok        : result list;
    disappoint: result list;
    errors    : result list;
    bad       : result list; (* mismatch *)
  }

  val make : Raw.t -> t

  val to_printbox : t -> PrintBox.t
  val to_printbox_bad : t -> PrintBox.t

  val is_ok : t -> bool

  val num_failed : t -> int

  val pp : t CCFormat.printer
  val pp_compact : t CCFormat.printer
end

module ResultsComparison : sig
  type t = {
    appeared: (Problem.t * Res.t) list;  (* new problems *)
    disappeared: (Problem.t * Res.t) list; (* problems that disappeared *)
    improved: (Problem.t * Res.t * Res.t) list;
    regressed: (Problem.t * Res.t * Res.t) list;
    mismatch: (Problem.t * Res.t * Res.t) list;
    same: (Problem.t * Res.t * float * float) list; (* same result *)
  }

  val compare : Raw.t -> Raw.t -> t

  val pp : t CCFormat.printer
  (** Display comparison in a readable way *)

  val pp_short : t CCFormat.printer
  (** Display comparison in a compact way *)

  val to_printbox : t -> PrintBox.t
  val to_printbox_short : t -> PrintBox.t
end

(** {2 Top Result}

    Main result of testing: a snapshot of the work done, + the analysis
    per prover *)

(* TODO: this should contain a UUID *)
type top_result = private {
  timestamp: float; (* timestamp *)
  events: Run_event.t list;
  total_wall_time: float;
  raw: Raw.t MStr.t lazy_t;
  analyze: Analyze.t MStr.t lazy_t;
}

module Top_result : sig
  type t = top_result

  val pp : t CCFormat.printer
  (** Full printer, including results *)

  val pp_header : t CCFormat.printer
  (** Print only meta-information: UUID and timestamp *)

  val pp_compact : t CCFormat.printer
  (** Print meta-informations + compact results *)

  val pp_bad : t CCFormat.printer

  (* FIXME:
     request a Uuid as a unique name (along with timestamp), provided
     from main. *)

  val make :
    ?total_wall_time:float -> ?timestamp:float ->
    Prover.name Run_result.t list ->
    t

  type comparison_result = {
    both: ResultsComparison.t MStr.t;
    left: Analyze.t MStr.t;
    right: Analyze.t MStr.t;
  }

  val compare : t -> t -> comparison_result

  val pp_comparison : comparison_result CCFormat.printer
  val comparison_to_printbox : ?short:bool -> comparison_result -> PrintBox.t

  type table_row = {
    tr_problem: string;
    tr_res: (string * Res.t * float) list; (* prover, result, time *)
  }

  type table = {
    t_meta: string;
    t_rows: table_row list;
    t_provers: string list;
  }

  val to_table : t -> table

  val table_to_csv : table -> Csv.t

  val table_to_printbox : table -> PrintBox.t

  val to_printbox_summary : t -> (string * PrintBox.t) list
  val to_printbox_table : t -> PrintBox.t
  val to_printbox_bad : t -> (string * PrintBox.t) list

  val to_csv : t -> Csv.t

  val to_csv_chan : out_channel -> t -> unit

  val to_csv_string : t -> string

  val to_csv_file : string -> t -> unit
  (** Write as CSV into given file *)

  val decode : t J.Decode.t

  val to_db : Db.t -> t -> unit or_error

  val of_db : Db.t -> t or_error
end

(** {2 Benchmark, within one Top Result} *)
module Bench : sig
  type per_prover = {
    stat: Raw.stat;
    sat: (string * float) list;
    unsat: (string * float) list;
  }

  type t = {
    from: top_result;
    per_prover: per_prover MStr.t;
  }

  val make : top_result -> t

  val pp : t CCFormat.printer
  (** Full printer that compares provers against one another *)
end


(* This file is free software. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

type 'a or_error = ('a, string) CCResult.t

module MStr = Misc.Str_map

type result = Event.prover Event.result

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

  val is_ok : t -> bool

  val num_failed : t -> int

  val pp : t CCFormat.printer
end

module Config : sig

  type expect =
    | Auto
    | Res of Res.t
    | Program of Prover.t

  type problem_set = {
    directory : string;
    pattern : string;
    expect : expect;
  }

  type t = {
    j: int; (* number of concurrent processes *)
    timeout: int; (* timeout for each problem *)
    memory: int;
    problems : problem_set list [@default []];
    provers: Prover.t list;
  }

  val make:
    ?j:int ->
    ?timeout:int ->
    ?memory:int ->
    ?dirs:problem_set list ->
    provers:Prover.t list ->
    unit -> t

  val update : ?j:int -> ?timeout:int -> ?memory:int -> t -> t
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
end

(** {2 Top Result}

    Main result of testing: a snapshot of the work done, + the analysis
    per prover *)

type top_result = private {
  timestamp: float; (* timestamp *)
  events: Event.t list;
  raw: Raw.t Prover.Map_name.t lazy_t;
  analyze: Analyze.t Prover.Map_name.t lazy_t;
}

module Top_result : sig
  type t = top_result

  val pp : t CCFormat.printer
  (** Full printer, including results *)

  val pp_header : t CCFormat.printer
  (** Print only meta-information: UUID and timestamp *)

  val merge : t -> t -> t

  val merge_l : t list -> t

  val make : ?timestamp:float -> Event.t list -> t

  val snapshot : ?meta:string -> t -> Event.Snapshot.t

  val of_snapshot : Event.Snapshot.t -> t

  val filter :
    provers:string list option ->
    dir:string list ->
    t -> t
  (** Filter the results by problem and by prover *)

  type comparison_result = {
    both: ResultsComparison.t Prover.Map_name.t;
    left: Analyze.t Prover.Map_name.t;
    right: Analyze.t Prover.Map_name.t;
  }

  val compare : t -> t -> comparison_result

  val pp_comparison : comparison_result CCFormat.printer

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

  val to_csv : t -> Csv.t

  val to_csv_chan : out_channel -> t -> unit

  val to_csv_string : t -> string

  val to_csv_file : string -> t -> unit
  (** Write as CSV into given file *)
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
    per_prover: per_prover Prover.Map_name.t;
  }

  val make : top_result -> t

  val pp : t CCFormat.printer
  (** Full printer that compares provers against one another *)
end

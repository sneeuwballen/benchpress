(* This file is free software. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

type 'a or_error = ('a, string) CCResult.t

module MStr = Misc.Str_map
module J = Misc.Json

type result = Run_event.prover Run_event.result

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

  val encode_stat : stat J.Encode.t
  val decode_stat : stat J.Decode.t

  val encode : t J.Encode.t
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

  val is_ok : t -> bool

  val num_failed : t -> int

  val pp : t CCFormat.printer
  val pp_compact : t CCFormat.printer
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
    problems : problem_set list;
    provers: Prover.t list;
    default_expect: Res.t option;
  }

  val make:
    ?j:int ->
    ?timeout:int ->
    ?memory:int ->
    ?dirs:problem_set list ->
    ?default_expect:Res.t ->
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
  events: Run_event.t list;
  total_wall_time: float;
  raw: Raw.t Prover.Map_name.t lazy_t;
  analyze: Analyze.t Prover.Map_name.t lazy_t;
}

module Top_result : sig
  type t = top_result

  val pp : t CCFormat.printer
  (** Full printer, including results *)

  val pp_header : t CCFormat.printer
  (** Print only meta-information: UUID and timestamp *)

  val pp_compact : t CCFormat.printer
  (** Print meta-informations + compact results *)

  (* FIXME:
     use meta everywhere;
     request a Uuid as a unique name (along with timestamp), provided
     from main. *)

  val merge : ?total_wall_time:float -> ?timestamp:float -> t -> t -> t

  val merge_l : ?total_wall_time:float -> ?timestamp:float -> t list -> t

  val make : ?total_wall_time:float -> ?timestamp:float -> Run_event.t list -> t

  val snapshot : ?meta:string -> t -> Run_event.Snapshot.t

  val of_snapshot : Run_event.Snapshot.t -> t

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

  val encode : t J.Encode.t
  val decode : t J.Decode.t
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

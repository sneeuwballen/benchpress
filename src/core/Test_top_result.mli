
(** Top Result for a benchmark run.

    Main result of testing: a snapshot of the work done, + the analysis
    per prover *)

open Common
open Test

type t = {
  meta: Test_metadata.t;
  provers: Prover.t list;
  events: Run_event.t list;
  stats: (Prover.name * Test_stat.t) list;
  analyze: (Prover.name * Test_analyze.t) list;
  db: Db.t; (* in-memory database *)
}

(** Filter on the list of all results *)
type tr_filter =
  | TRF_all
  | TRF_different (** different results *)
  | TRF_bad (** inconsistent results *)

val string_of_trf : tr_filter -> string

val pp : t Fmt.printer
(** Full printer, including results *)

val pp_header : t Fmt.printer
(** Print only meta-information: UUID and timestamp *)

val pp_compact : t Fmt.printer
(** Print meta-informations + compact results *)

val pp_bad : t Fmt.printer

val is_ok : t -> bool

val make :
  analyze_full:bool ->
  meta:Test_metadata.t ->
  provers:Prover.t list ->
  Run_event.t list ->
  t
(** Make from a list of results *)

val of_db :
  analyze_full:bool ->
  Db.t -> t
(** Parse from a DB *)

val db_prepare : Db.t -> unit

val to_db : Db.t -> t -> unit
(** Dump into the DB *)

val stat : t -> (Prover.name * Test_stat.t) list
(** Compute or retrieve stats *)

val analyze : t -> (Prover.name * Test_analyze.t) list

val to_compact_result : t -> Test_compact_result.t

(* TODO: move to another file
   type comparison_result = {
   both: ResultsComparison.t MStr.t;
   left: Test_analyze.t MStr.t;
   right: Test_analyze.t MStr.t;
   }

   val compare : t -> t -> comparison_result

   val pp_comparison : comparison_result Fmt.printer
   val comparison_to_printbox : ?short:bool -> comparison_result -> PrintBox.t
*)

type table_row = {
  tr_problem: string;
  tr_res: (string * Res.t * float) list; (* prover, result, time *)
}

type table = {
  t_meta: string;
  t_rows: table_row list;
  t_provers: string list;
}

val db_to_table :
  ?offset:int -> ?page_size:int ->
  ?provers:string list ->
  ?filter_pb:string ->
  ?filter_res:tr_filter ->
  Db.t -> table
val to_table :
  ?offset:int -> ?page_size:int ->
  ?provers:string list ->
  t -> table

val table_to_csv : table -> Csv.t

val table_to_printbox :
  ?link_pb:path_linker -> ?link_res:prover_path_res_linker ->
  table -> PrintBox.t

val to_printbox_summary : t -> PrintBox.t
val to_printbox_stat : t -> PrintBox.t

val to_printbox_table :
  ?offset:int -> ?page_size:int ->
  ?link_pb:path_linker -> ?link_res:prover_path_res_linker ->
  t -> PrintBox.t

val db_to_printbox_table :
  ?offset:int -> ?page_size:int ->
  ?link_pb:path_linker -> ?link_res:prover_path_res_linker ->
  ?filter_pb:string ->
  ?filter_res:tr_filter ->
  Db.t -> PrintBox.t

val to_printbox_bad : t -> (string * PrintBox.t) list
val to_printbox_errors : t -> (string * PrintBox.t) list

val db_to_csv : ?provers:string list -> Db.t -> Csv.t
val to_csv : ?provers:string list -> t -> Csv.t

val to_csv_chan : ?provers:string list -> out_channel -> t -> unit

val db_to_csv_string : ?provers:string list -> Db.t -> string
val to_csv_string : ?provers:string list -> t -> string

val to_csv_file : ?provers:string list -> string -> t -> unit
(** Write as CSV into given file *)

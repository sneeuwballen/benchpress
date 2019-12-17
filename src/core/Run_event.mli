(* This file is free software. See file "license" for more details. *)

(** {1 Event Stored on Disk or Transmitted on Network} *)

module Db = Sqlite3_utils
module J = Misc.Json

type 'a or_error = ('a, string) CCResult.t

type prover  = Prover.t
type checker = unit

type t =
  | Prover_run of Prover.name Run_result.t
  | Checker_run of checker Run_result.t

type event = t

val mk_prover : Prover.name Run_result.t -> t
val mk_checker : checker Run_result.t -> t

val pp : t CCFormat.printer

val decode : t J.Decode.t

val db_prepare : Db.t -> unit or_error
val to_db_prover_result : Db.t -> Prover.name Run_result.t -> unit or_error
val to_db: Db.t -> t -> unit or_error

val of_db_map : Db.t -> f:(Prover.name Run_result.t -> 'a) -> 'a list or_error
val of_db_l : Db.t -> Prover.name Run_result.t list or_error

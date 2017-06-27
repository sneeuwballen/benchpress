
(* This file is free software. See file "license" for more details. *)

(** {1 Event Stored on Disk or Transmitted on Network} *)

type 'a or_error = ('a, string) CCResult.t

type raw_result = {
  (* Raw output *)
  errcode: int;
  stdout: string;
  stderr: string;

  (* Time used *)
  rtime : float;
  utime : float;
  stime : float;
}

type prover  = Prover.t
type checker = unit

type +'a result = {
  program : 'a;
  problem : Problem.t;
  raw : raw_result;
}

val program : 'a result -> 'a
val problem : _ result -> Problem.t
val raw : _ result -> raw_result

val analyze_p : prover result -> Res.t

type t =
  | Prover_run of prover result
  | Checker_run of checker result

type event = t

val mk_prover : prover result -> t
val mk_checker : checker result -> t

val pp : t CCFormat.printer

type snapshot = private {
  timestamp: float;
  events: t list;
  meta: string; (* additional metadata *)
}

module Snapshot : sig
  type t = snapshot

  val make : ?meta:string -> ?timestamp:float -> event list -> t

  val provers : t -> Prover.Set.t

  val pp : t CCFormat.printer
end

type snapshot_meta = private {
  s_timestamp: float;
  s_meta: string;
  s_provers: Prover.Set.t;
  s_len: int;
}

module Meta : sig
  type t = snapshot_meta

  val provers : t -> Prover.Set.t
  val timestamp : t -> float
  val length : t -> int

  val pp : t CCFormat.printer
end

val meta : snapshot -> snapshot_meta


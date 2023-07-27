(** Ulimit handling of limits *)

type conf = { time: bool; memory: bool; stack: bool }
(** The configuration of ulimit, aka which limits to enforce
    via ulimit. *)

val hash : conf -> int
val equal : conf -> conf -> bool

val compare : conf -> conf -> int
(** Usual functions *)

val pp : conf CCFormat.printer
(** Printer *)

val mk : time:bool -> memory:bool -> stack:bool -> conf
(** Create a ulimit conf. *)

val prefix_cmd : conf:conf -> limits:Limit.All.t -> cmd:string -> string
(** Given a ulimit configuration and a set of limits, prefix the given command
    with the adequate `ulimit` calls, if necessary, to enforce the limits. *)

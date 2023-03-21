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

val cmd : conf:conf -> limits:Limit.All.t -> string option
(** Given a ulimit conf, and a set of limits, return an adequate command
    (if any is necessary) to enforce the limits using ulimit. *)

val prefix_cmd : ?prefix:string -> cmd:string -> unit -> string
(** Adequately prefix the given command with the optional given prefix command. *)

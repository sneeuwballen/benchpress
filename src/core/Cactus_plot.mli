(** Cactus plots for a single run

    This makes it easier to compare provers, in a given run, at a glance. *)

open Misc

type t

val of_db : Db.t -> t
val of_file : string -> t
val combine : (string * t) list -> t
val show : t -> unit
val save_to_file : t -> string -> unit
val to_png : t -> string

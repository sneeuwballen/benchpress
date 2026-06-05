type pos = { line: int; col: int }
type t = { file: string; start: pos; stop: pos }

val none : t
(** Dummy location, used when no real position is available. *)

val pp : Format.formatter -> t -> unit
(** Format as "file:start_line:start_col-stop_line:stop_col" or compact
    "file:line" if start/stop on same line. *)


type span
val enter : unit -> span

val exit :
  ?args:(string*string) list ->
  string -> span -> unit

val with_:
  ?args:(string*string) list ->
  string -> (unit -> 'a) -> 'a

val with1:
  ?args:(string*string) list ->
  string -> ('a -> 'b) -> 'a -> 'b

val enable : unit -> unit

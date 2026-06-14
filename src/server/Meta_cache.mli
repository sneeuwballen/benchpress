type t

val create : sw:Eio.Switch.t -> path:string -> t
val find : t -> string -> Test_metadata.t
val find_if_loaded : t -> string -> Test_metadata.t option
val find_similar_runs : t -> string -> string list
val remove : t -> string -> unit

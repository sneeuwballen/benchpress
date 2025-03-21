type t

val create : path:string -> t
val find : t -> string -> Test_metadata.t
val find_if_loaded : t -> string -> Test_metadata.t option

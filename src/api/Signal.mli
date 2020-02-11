
(** {1 Signal}

    The observer pattern *)

type 'a t

val create : unit -> 'a t

module Sink : sig
  type 'a t
  val send : 'a t -> 'a -> unit
end

module Source : sig
  type 'a t

  val on : 'a t -> ('a -> bool) -> unit

  val on_always : 'a t -> ('a -> unit) -> unit

  val once : 'a t -> ('a -> unit) -> unit
end

include module type of Sink with type 'a t := 'a t
include module type of Source with type 'a t := 'a t

val sink : 'a t -> 'a Sink.t

val source : 'a t -> 'a Source.t

val chain : 'a Source.t -> 'a Sink.t -> unit

val chain_map : f:('a -> 'b) -> 'a Source.t -> 'b Sink.t -> unit


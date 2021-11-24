
type 'a t = ('a, Error.t) result

val return : 'a -> 'a t
val fail : ?loc:Loc.t -> string -> _ t
val failf : ?loc:Loc.t -> ('a, Format.formatter, unit, _ t) format4 -> 'a
val wrap : ?loc:Loc.t -> string -> 'a t -> 'a t
val wrapf : ?loc:Loc.t -> ('a, Format.formatter, unit, 'b t -> 'b t) format4 -> 'a

val of_exn : ?loc:Loc.t -> exn -> _ t

val map : ('a -> 'b) -> 'a t -> 'b t
val map_l : ('a -> 'b t) -> 'a list -> 'b list t
val fold_l : ('b -> 'a -> 'b t) -> 'b -> 'a list -> 'b t
val flatten_l : 'a t list -> 'a list t

val map_err : (Error.t -> Error.t) -> 'a t -> 'a t

module Infix : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end
include module type of Infix

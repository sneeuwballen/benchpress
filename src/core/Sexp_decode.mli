
open Common

type sexp = Sexp_loc.t

type +'a t
type 'a m = 'a t
val return : 'a -> 'a t

val fail : string -> _ t
val failf : ('a, Format.formatter, unit, 'b t) format4 -> 'a

module Infix : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end
include module type of Infix

val value : sexp t
val atom : string t
val list : sexp list t
val list_of : 'a t -> 'a list t

val any_of : msg:string -> 'a t list -> 'a t

val fix : ('a t -> 'a t) -> 'a t
val sub : 'a t -> sexp -> 'a t

val applied : (string * sexp list) t

val try_apply : string -> (sexp list -> 'a t) -> 'a t -> 'a t
(** [try_apply f ok else_] tries to parse the sexp [(f x1…xn)],
    in which case it calls [ok [x1;…;xn]]. Otherwise it
    calls [else_] on the root sexp. *)

(*
val pair_str : string -> 'a t -> 'a t
(** [pair_str "foo" d] decodes [(foo x)] using [d] on [x] *)
   *)

module Fields : sig
  type t

  val field : t -> string -> 'a m -> 'a m
  val field_opt : t -> string -> 'a m -> 'a option m
  val check_no_field_left : t -> unit m
end

val fields : Fields.t t

type err

val run : 'a t -> sexp -> ('a, err) result
val run' : 'a t -> sexp -> ('a, string) result

module Err : sig
  type t = err
  val pp : t Fmt.printer
  val to_string : t -> string
  val sexp : t -> sexp
  val loc : t -> Loc.t
end


open Common

type sexp = Sexp_loc.t

type +'a t
type 'a m = 'a t
val return : 'a -> 'a t

val fail : string -> _ t
val failf : ((('a, Format.formatter, unit, string) format4 -> 'a) -> string) -> 'b m

module Infix : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end
include module type of Infix

val value : sexp t
val value_loc : Loc.t t
val atom : string t
val list : sexp list t
val list_of : 'a t -> 'a list t
val pair : 'a t -> 'b t -> ('a * 'b) t

val string : string t
(** Alias to {!atom} *)

val int : int t
val bool : bool t

val atom_or_atom_list : string list t
(** Parse either ["foo"] or [("a" "b" "c")] *)

val keyword : msg:string -> (string * 'a) list -> 'a t

val is_atom : bool t

val is_list : bool t

val succeeds: 'a t -> bool t
(** [succeeds d] returns [true] if [d] parses the S-expr, and [false] otherwise. *)

val is_applied : string -> bool t
(** [is_applied "foo"] is the recognizer that
    accepts expressions of the form [("foo" …)] *)

val try_succeed : 'a t -> (bool t * 'a t)
(** [try_succeed d] is [succeeds d, d] *)

val try_l : msg:string -> (bool t * 'a t) list -> 'a t
(** [try_l ~msg l] parses a sexp by trying each case in [l] successively,
    until one succeeds.
    A case is a pair of a recognizer and a parser. If the recognizer succeeds,
    then this case wins, and [try_l l] behaves like the case's parser;
    if the recognizer fails, the case is discarded and the next case is tried.
    @param msg error message if no case recognizes the parser. *)

val map_l : ('a -> 'b t) -> 'a list -> 'b list t
val fold_l : ('b -> 'a -> 'b t) -> 'b -> 'a list -> 'b t

val fix : ('a t -> 'a t) -> 'a t
val sub : 'a t -> sexp -> 'a t

val applied : string -> 'a t -> 'a list t
val applied0 : string -> unit t
val applied1 : string -> 'a t -> 'a t
val applied2 : string -> 'a t -> 'b t -> ('a * 'b) t
val applied3 : string -> 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

val try_apply : string -> (sexp list -> 'a t) -> 'a t -> 'a t
(** [try_apply f ok else_] tries to parse the sexp [(f x1…xn)],
    in which case it calls [ok [x1;…;xn]]. Otherwise it
    calls [else_] on the root sexp. *)

module Fields : sig
  type t
  (** A mutable collection of pairs (key-value), used to represent
      records *)

  val field : t -> string -> 'a m -> 'a m
  (** [field m name d] gets and consume the pair [(<name>, v)] from [m],
      using [d] to decode [v].
      This fails if the field is not present. It removes it from [m]
      on success, so must never be called twice with the same field name
      on a given collection. *)

  val field_opt : t -> string -> 'a m -> 'a option m
  (** Same as {!field} but doesn't fail if the field is absent. *)

  val check_no_field_left : t -> unit m
  (** Check that all fields have been consumed by {!field} and {!field_opt}
      above.
      This fails if there are some pairs that were not used at all, which
      is useful to detect typos and unrecognized inputs. *)
end

val fields : Fields.t t
(** Parses a list of pairs [((a b) (c d) …)] as a record. *)

val applied_fields : string -> Fields.t t
(** [applied_fields "foo"] accepts [("foo" (a b) (c d) …)]
    and returns the corresponding list of fields. *)

type err

val run : 'a t -> sexp -> ('a, err) result
val run' : 'a t -> sexp -> ('a, string) result

module Err : sig
  type t = err
  val loc : t -> Loc.t
  val to_error : t -> Error.t
end

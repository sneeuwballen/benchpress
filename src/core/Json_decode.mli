(* This file is free software. See file "license" for more details. *)

type value = Config_value.value
type path = Root | Field of string * path | Index of int * path

val path_to_string : path -> string

type err = { msg: string; path: path; value: value; ctx_of: err option }

exception Decode_error of err

type +'a t

val return : 'a -> 'a t
val fail : string -> 'a t
val failf : ('a, unit, string, 'b t) format4 -> 'a
val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val string : string t
val int : int t
val bool : bool t
val option : 'a t -> 'a option t
val list : 'a t -> 'a list t
val field : string -> 'a t -> 'a t
val field_opt : string -> 'a t -> 'a option t
val field_or : string -> default:'a -> 'a t -> 'a t
val value : value t
val sub : 'a t -> value -> 'a t
val fix : ('a t -> 'a t) -> 'a t
val run : 'a t -> value -> ('a, err) result
val run_exn : 'a t -> value -> 'a

module Err : sig
  type t = err

  val to_string : t -> string
  val pp : t Fmt.t
end

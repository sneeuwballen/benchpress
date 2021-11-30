open Common

type t =
  | Valid
  | Invalid
  | Unknown of string

val pp : t Fmt.printer

val of_string : string -> t Or_error.t
val to_string : t -> string

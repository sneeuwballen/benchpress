open Common

type t = Valid | Invalid | Unknown of string

val pp : t Fmt.printer
val of_string : string -> t
val to_string : t -> string

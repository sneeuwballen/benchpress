(* This file is free software. See file "license" for more details. *)

module Fmt = CCFormat
type 'a or_error = ('a, string) CCResult.t

type path = string
type t = {
  name: path;  (* filename *)
  expected: Res.t; (* result expected *)
}

val make : string -> Res.t -> t
(** Make a problem. *)

val find_expect :
  ?default_expect:Res.t ->
  expect:Dir.expect ->
  path ->
  Res.t or_error
(** FInd the expected result for this given problem *)

val make_find_expect :
  expect:Dir.expect ->
  path ->
  t or_error
(** [make_find_expect ~expect file] tries to find the expected
    result of [file] using [expect], and
    makes a problem if it finds the result
    @param expect the method for finding expected result *)

val basename : t -> string
(** Returns the basename of a problem *)

val same_name : t -> t -> bool
val hash_name : t -> int
val compare_name : t -> t -> int
(** Compare the names of problems. *)

val compare_res : t -> Res.t -> [`Same | `Improvement | `Mismatch | `Disappoint | `Error]
(** [compare_res pb res] compares the expected result of [pb] to
    the actual result [res], yielding one of:

    {ul
      {- `Same if they coincide}
      {- `Mismatch if they do not match in an unsound way (error)}
      {- `Disappoint if the result is not incorrect, but less good than expected}
      {- `Improvement if unknown was expected, but sat|unsat was found}
      {- `Error if the actual result is an error but not the expect result}
    }
*)

val pp : t Fmt.printer
val name : t -> string
val to_string : t -> string


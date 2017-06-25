
(* This file is free software. See file "license" for more details. *)

type 'a or_error = ('a, string) CCResult.t

type t = Problem.t
type path = string

val find_expect :
  expect:Test.Config.expect ->
  path ->
  Res.t or_error
(** FInd the expected result for this given problem *)

val make :
  find_expect:(path -> Res.t or_error) ->
  path ->
  Problem.t or_error
(** [make ~find_expect file] tries to find the expected result of [file], and
    makes a problem if it finds the result
    @param find_expect the function to obtain the actual expected result *)

val of_dir :
  filter:(string -> bool) ->
  path ->
  path list or_error
(** Traverse the directory and returns all files that match the given filter *)

module Set : sig
  type t = Problem.problem_set

  val size : t -> int

  val make:
    find_expect:(path -> Res.t or_error) ->
    string list ->
    t or_error
    (** Build a set of problems out of file names *)

  val of_dir :
    expect:Test.Config.expect ->
    filter:(string -> bool) ->
    string ->
    t or_error
    (** Traverse the directory and makes a problem set out of every
        file it contains.
        @param filter if present, only files that satisfy the predicate are used *)

  val pp : t CCFormat.printer
end

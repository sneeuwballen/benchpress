
(* This file is free software. See file "license" for more details. *)

(** {1 Run Tests} *)

type 'a or_error = ('a, string) CCResult.t
type path = string

val config_of_file : ?profile:string -> string -> Test.Config.t or_error

val config_of_config : ?profile:string -> Config.t -> string list -> Test.Config.t or_error
(** [config_of_config ?profile conf dirs] makes a test config out of the
    raw configuration.
    It will gather all problems present in one of the [dirs] that the
    configuration matches.
    @param profile if present, look for the test configuration named [profile]
    instead of "test" *)

val pp_result : Test.result -> unit

val run :
  ?on_solve:(Test.result -> unit) ->
  ?on_done:(Test.top_result -> unit) ->
  ?timeout:int ->
  ?memory:int ->
  provers:Prover.t list ->
  expect:Test.Config.expect ->
  config:Test.Config.t ->
  path list ->
  Test.top_result or_error
(** Run the given prover(s) on the given problem set, obtaining results
    after all the problems have been dealt with.
    @param caching if true, use Maki for caching results (default true)
    @param on_solve called whenever a single problem is solved *)


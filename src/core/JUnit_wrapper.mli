
(* This file is free software. See file "license" for more details. *)

(** {1 Junit wrapper} *)

val test_analyze : Test.Analyze.t -> Junit.Testsuite.t
(** Converts the results into a junit testsuite *)

val junit_to_file : Junit.Testsuite.t list -> string -> unit
(** [to_junit_file j file] writes the testsuite [j] into given file *)


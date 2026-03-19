(** Logging setup *)

module Log_report : sig
  val reporter : unit -> Logs.reporter
end

val setup_logs : Logs.level option -> unit
(** Setup the logging infrastructure *)

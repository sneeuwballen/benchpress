(** Logging setup *)

module Log_report : sig
  val reporter : unit -> Logs.reporter
end

(** Setup the logging infrastructure *)
val setup_logs : Logs.level option -> unit

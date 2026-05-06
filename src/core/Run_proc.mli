val with_proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t -> (unit -> 'a) -> 'a
(** [with_proc_mgr mgr f] runs [f] with [mgr] available to [run] via fiber-local
    storage. Must be called before [run] is invoked on any fiber in scope. *)

val run : string -> Run_proc_result.t
(** [run cmd] runs the shell command [cmd] via [/bin/sh -c] and returns its
    output. Requires a process manager set via [with_proc_mgr]. *)

val run_argv : string array -> Run_proc_result.t
(** [run_argv argv] spawns [argv.(0)] directly (no shell) and returns its
    output. Requires a process manager set via [with_proc_mgr]. *)

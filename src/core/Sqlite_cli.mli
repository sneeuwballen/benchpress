(** Read old .sqlite result files by shelling out to the sqlite3 CLI.
    stdout/stderr of individual runs are not preserved (set to empty). *)

val read_events : string -> Run_event.t list
(** Read all prover and checker run events from a .sqlite file. *)

val read_meta : string -> Test_metadata.t
(** Read summary metadata from a .sqlite file (uuid, timestamp, n_results,
    provers). [n_bad] is set to 0; [dirs] is set to [[]]. Compute them from
    events if needed. *)

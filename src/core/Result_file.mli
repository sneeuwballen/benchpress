(** Read/write benchmark result files in .zip format. Zip layout: events.jsonl —
    first entry; Event JSON lines (RunStart, ProverDone, ...) data.<sha256> —
    raw blob for each unique large stdout/stderr content *)

type writer

val open_write : string -> meta:Test_metadata.t -> writer
(** Open a .zip file for writing and immediately emit a RunStart event. *)

val write_event : writer -> Run_event.t -> unit
(** Append a ProverDone or CheckerDone JSON line; registers any large blobs. *)

val close_write :
  writer ->
  total_wall_time:float ->
  n_results:int ->
  n_bad:int ->
  dirs:string list ->
  unit
(** Emit a RunDone event, write manifest.json, finalize and close the zip. *)

val read_events : string -> Run_event.t list
(** Read all result events from events.jsonl in the zip. Blob data (for large
    stdout/stderr) is fetched from the zip on demand. *)

val read_meta : string -> Test_metadata.t
(** Read only the RunStart metadata from events.jsonl. *)

module Log = (val Logs.src_log (Logs.Src.create "benchpress.result-file"))

open struct
  module Trace = Trace_core

  let ( let@ ) = ( @@ )
end

(* --- Writing --- *)

type writer = {
  mutable events_buf: Buffer.t;
  (* hash -> raw bytes; collected during write, added as zip entries at close *)
  blobs: (string, bytes) Hashtbl.t;
  zip_path: string;
  meta: Test_metadata.t;
}

let open_write zip_path ~meta : writer =
  let w =
    {
      events_buf = Buffer.create (1024 * 64);
      blobs = Hashtbl.create 16;
      zip_path;
      meta;
    }
  in
  (* Write RunStart as first line *)
  let rs =
    Benchpress_core.make_run_start
      ~uuid:(Uuidm.to_string meta.uuid)
      ~dirs:meta.dirs ~provers:meta.provers ()
  in
  (match meta.timestamp with
  | Some t -> Benchpress_core.run_start_set_timestamp rs t
  | None -> ());
  let ev = Benchpress_core.Run_start rs in
  Buffer.add_string w.events_buf
    (Yojson.Basic.to_string (Benchpress_core.encode_json_event ev));
  Buffer.add_char w.events_buf '\n';
  w

(* Register a blob if it's a sha256 ref; return the data_ref for embedding *)
let register_blob w (raw_bytes : string) : Benchpress_core.data_ref =
  let b = Bytes.of_string raw_bytes in
  let dr = Blob_ref.of_bytes b in
  (match dr with
  | Benchpress_core.Sha256ref h ->
    if not (Hashtbl.mem w.blobs h) then Hashtbl.add w.blobs h b
  | Benchpress_core.Inline _ -> ());
  dr

let write_event (w : writer) (ev : Run_event.t) : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "res-file.write-event" in
  (* For large outputs, register blobs and encode as sha256 refs *)
  let ev_pb =
    match ev with
    | Run_event.Prover_run r ->
      let raw = r.Run_result.raw in
      let stdout = register_blob w raw.Run_proc_result.stdout in
      let stderr = register_blob w raw.Run_proc_result.stderr in
      let labels =
        match r.Run_result.res with
        | Res.Tag s -> [ s ]
        | _ -> []
      in
      Benchpress_core.Prover_done
        (Benchpress_core.make_prover_done ~prover:r.Run_result.program
           ~problem:
             (Benchpress_core.make_problem
                ~name:r.Run_result.problem.Problem.name
                ~expected:(Res.to_string r.Run_result.problem.Problem.expected)
                ())
           ~res:(Res.to_string r.Run_result.res)
           ~labels
           ~timeout_s:
             (Int64.of_int (Limit.Time.as_int Seconds r.Run_result.timeout))
           ~raw:
             (Benchpress_core.make_run_proc_result
                ~errcode:(Int32.of_int raw.Run_proc_result.errcode)
                ~stdout ~stderr ~rtime:raw.Run_proc_result.rtime
                ~utime:raw.Run_proc_result.utime
                ~stime:raw.Run_proc_result.stime ())
           ())
    | Run_event.Checker_run r ->
      let raw = r.Run_result.raw in
      let stdout = register_blob w raw.Run_proc_result.stdout in
      let stderr = register_blob w raw.Run_proc_result.stderr in
      let p, c = r.Run_result.program in
      Benchpress_core.Checker_done
        (Benchpress_core.make_checker_done ~prover:p ~checker:c
           ~problem:
             (Benchpress_core.make_problem
                ~name:r.Run_result.problem.Problem.name
                ~expected:(Res.to_string r.Run_result.problem.Problem.expected)
                ())
           ~proof_check_res:(Proof_check_res.to_string r.Run_result.res)
           ~raw:
             (Benchpress_core.make_run_proc_result
                ~errcode:(Int32.of_int raw.Run_proc_result.errcode)
                ~stdout ~stderr ~rtime:raw.Run_proc_result.rtime
                ~utime:raw.Run_proc_result.utime
                ~stime:raw.Run_proc_result.stime ())
           ())
  in
  Buffer.add_string w.events_buf
    (Yojson.Basic.to_string (Benchpress_core.encode_json_event ev_pb));
  Buffer.add_char w.events_buf '\n'

let format_rfc3339 (t : float) : string =
  match Ptime.of_float_s t with
  | None -> ""
  | Some pt -> Ptime.to_rfc3339 pt

let parse_rfc3339 (s : string) : float option =
  if s = "" then
    None
  else (
    match Ptime.of_rfc3339 s with
    | Ok (pt, _, _) -> Some (Ptime.to_float_s pt)
    | Error _ -> None
  )

let make_manifest_json w ~total_wall_time ~n_results ~n_bad ~dirs : string =
  let@ _sp =
    Trace.with_span ~__FILE__ ~__LINE__ "res-file.make-manifest-json"
  in
  let meta = w.meta in
  let m =
    Benchpress_core.make_manifest
      ~uuid:(Uuidm.to_string meta.uuid)
      ~timestamp:
        (match meta.timestamp with
        | Some t -> format_rfc3339 t
        | None -> "")
      ~provers:meta.provers ~total_wall_time ~n_results:(Int32.of_int n_results)
      ~n_bad:(Int32.of_int n_bad) ~dirs ()
  in
  Yojson.Basic.to_string (Benchpress_core.encode_json_manifest m)

let close_write w ~total_wall_time ~n_results ~n_bad ~dirs =
  let rd =
    Benchpress_core.make_run_done ~total_wall_time
      ~n_results:(Int32.of_int n_results) ~n_bad:(Int32.of_int n_bad) ()
  in
  Buffer.add_string w.events_buf
    (Yojson.Basic.to_string
       (Benchpress_core.encode_json_event (Benchpress_core.Run_done rd)));
  Buffer.add_char w.events_buf '\n';
  let oz = Zip.open_out w.zip_path in
  Zip.add_entry (Buffer.contents w.events_buf) oz "events.jsonl";
  Zip.add_entry
    (make_manifest_json w ~total_wall_time ~n_results ~n_bad ~dirs)
    oz "manifest.json";
  Hashtbl.iter
    (fun hash b -> Zip.add_entry (Bytes.to_string b) oz ("data." ^ hash))
    w.blobs;
  Zip.close_out oz;
  Log.info (fun k ->
      k "wrote %d blobs to %s" (Hashtbl.length w.blobs) w.zip_path)

(* --- Reading --- *)

let read_zip_entry zip_path =
  let iz = Zip.open_in zip_path in
  let cache : (string, bytes) Hashtbl.t = Hashtbl.create 8 in
  let reader name =
    match Hashtbl.find_opt cache name with
    | Some b -> b
    | None ->
      let@ _sp =
        Trace.with_span ~__FILE__ ~__LINE__ "res-file.zip.read-entry"
      in
      Trace.add_data_to_span _sp [ "name", `String name ];
      (try
         let entry = Zip.find_entry iz name in
         let s = Zip.read_entry iz entry in
         let b = Bytes.of_string s in
         Hashtbl.add cache name b;
         b
       with Not_found -> Bytes.empty)
  in
  iz, reader

let read_events zip_path : Run_event.t list =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "res-file.zip.read-events" in
  let iz, read_zip_entry = read_zip_entry zip_path in
  try
    let entry = Zip.find_entry iz "events.jsonl" in
    let content = Zip.read_entry iz entry in
    let lines = String.split_on_char '\n' content in
    let events =
      List.filter_map
        (fun line ->
          if String.length line = 0 then
            None
          else (
            try Run_event.of_json_line ~read_zip_entry line
            with exn ->
              Log.warn (fun k ->
                  k "skipping unparseable event line: %s"
                    (Printexc.to_string exn));
              None
          ))
        lines
    in
    Zip.close_in iz;
    events
  with exn ->
    Zip.close_in iz;
    Error.failf "reading events from %s: %s" zip_path (Printexc.to_string exn)

let read_meta zip_path : Test_metadata.t =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "res-file.zip.read-meta" in
  let iz = Zip.open_in zip_path in
  try
    let entry = Zip.find_entry iz "manifest.json" in
    let s = Zip.read_entry iz entry in
    Zip.close_in iz;
    let m = Benchpress_core.decode_json_manifest (Yojson.Basic.from_string s) in
    {
      Test_metadata.uuid =
        (match Uuidm.of_string m.Benchpress_core.uuid with
        | Some u -> u
        | None -> Uuidm.nil);
      timestamp = parse_rfc3339 m.Benchpress_core.timestamp;
      total_wall_time = Some m.Benchpress_core.total_wall_time;
      n_results = Int32.to_int m.Benchpress_core.n_results;
      n_bad = Int32.to_int m.Benchpress_core.n_bad;
      dirs = m.Benchpress_core.dirs;
      provers = m.Benchpress_core.provers;
    }
  with exn ->
    Zip.close_in iz;
    Error.failf "reading manifest from %s: %s" zip_path (Printexc.to_string exn)

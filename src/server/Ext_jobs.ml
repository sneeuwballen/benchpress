module Api = Benchpress_api_proto.Benchpress_api

type job = { report: Api.progress_report; mutable last_seen: float }
type t = (string, job) Hashtbl.t

let create () : t = Hashtbl.create 8

let apply_report t ~now (r : Api.progress_report) =
  let uuid = r.Api.uuid in
  let job =
    match Hashtbl.find_opt t uuid with
    | Some j -> j
    | None ->
      let j = { report = r; last_seen = now } in
      Hashtbl.add t uuid j;
      Logs.info (fun k ->
          k "external job registered: %s (%ld tasks)" uuid r.Api.total_tasks);
      j
  in
  Api.progress_report_set_total_tasks job.report r.Api.total_tasks;
  Api.progress_report_set_done_tasks job.report r.Api.done_tasks;
  Api.progress_report_set_active job.report r.Api.active;
  Api.progress_report_set_finished job.report r.Api.finished;
  Api.progress_report_set_start_ts job.report r.Api.start_ts;
  if r.Api.stats <> "" then Api.progress_report_set_stats job.report r.Api.stats;
  job.last_seen <- now

let expire t ~now =
  let timeout = 600.0 in
  let expired = ref [] in
  Hashtbl.iter
    (fun uuid job ->
      if (not job.report.Api.finished) && now -. job.last_seen > timeout then
        expired := uuid :: !expired)
    t;
  List.iter
    (fun uuid ->
      Logs.info (fun k -> k "external job expired (timeout): %s" uuid);
      Hashtbl.remove t uuid)
    !expired

let all_active t : job list =
  Hashtbl.fold (fun _ j acc -> j :: acc) t []
  |> List.filter (fun j -> not j.report.Api.finished)
  |> List.sort (fun a b -> compare a.report.Api.start_ts b.report.Api.start_ts)

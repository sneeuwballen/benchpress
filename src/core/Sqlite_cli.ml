open Common
module Log = (val Logs.src_log (Logs.Src.create "benchpress.sqlite-cli"))

let run_query db_path sql =
  let cmd =
    Printf.sprintf "sqlite3 -json %s %s" (Filename.quote db_path)
      (Filename.quote sql)
  in
  let ic = Unix.open_process_in cmd in
  let s = CCIO.read_all ic |> String.trim in
  (match Unix.close_process_in ic with
  | Unix.WEXITED (0 | 1) -> ()
  | _ -> Log.warn (fun k -> k "sqlite3 non-zero exit for: %s" sql));
  if s = "" || s = "[]" then
    []
  else (
    try
      match Yojson.Basic.from_string s with
      | `List rows -> rows
      | _ -> []
    with exn ->
      Log.warn (fun k ->
          k "sqlite3 output parse failure: %s" (Printexc.to_string exn));
      []
  )

let str row key =
  try Yojson.Basic.Util.(row |> member key |> to_string) with _ -> ""

let int_ row key =
  try Yojson.Basic.Util.(row |> member key |> to_int) with _ -> 0

let float_ row key =
  try Yojson.Basic.Util.(row |> member key |> to_float) with _ -> 0.

let read_prover_events db_path : Run_event.t list =
  let sql =
    "SELECT prover,file,res,file_expect,timeout,errcode,rtime,utime,stime FROM \
     prover_res"
  in
  run_query db_path sql
  |> List.filter_map (fun row ->
         try
           let prover = str row "prover" in
           let file = str row "file" in
           let res = Res.of_string ~tags:[] (str row "res") in
           let expected = Res.of_string ~tags:[] (str row "file_expect") in
           let timeout = int_ row "timeout" in
           let errcode = int_ row "errcode" in
           let rtime = float_ row "rtime" in
           let utime = float_ row "utime" in
           let stime = float_ row "stime" in
           Some
             (Run_event.mk_prover
                (Run_result.make prover
                   { Problem.name = file; expected }
                   ~timeout:(Limit.Time.mk ~s:timeout ())
                   ~res
                   {
                     Run_proc_result.errcode;
                     stdout = "";
                     stderr = "";
                     rtime;
                     utime;
                     stime;
                   }))
         with exn ->
           Log.warn (fun k ->
               k "skipping prover row: %s" (Printexc.to_string exn));
           None)

let table_exists db_path name =
  let sql =
    Printf.sprintf
      "SELECT name FROM sqlite_master WHERE type='table' AND name='%s'" name
  in
  run_query db_path sql <> []

let read_checker_events db_path : Run_event.t list =
  if not (table_exists db_path "proof_check_res") then
    []
  else (
    let sql =
      "SELECT p.prover,p.file,e.file_expect,p.checker,p.res,p.rtime FROM \
       proof_check_res p, (SELECT DISTINCT file,file_expect FROM prover_res) \
       AS e WHERE p.file=e.file"
    in
    run_query db_path sql
    |> List.filter_map (fun row ->
           try
             let prover = str row "prover" in
             let file = str row "file" in
             let expected = Res.of_string ~tags:[] (str row "file_expect") in
             let checker = str row "checker" in
             let res = Proof_check_res.of_string (str row "res") in
             let rtime = float_ row "rtime" in
             Some
               (Run_event.mk_checker
                  (Run_result.make (prover, checker)
                     { Problem.name = file; expected }
                     ~timeout:(Limit.Time.mk ~s:0 ()) ~res
                     {
                       Run_proc_result.errcode = 0;
                       stdout = "";
                       stderr = "";
                       rtime;
                       utime = 0.;
                       stime = 0.;
                     }))
           with exn ->
             Log.warn (fun k ->
                 k "skipping checker row: %s" (Printexc.to_string exn));
             None)
  )

let read_events db_path : Run_event.t list =
  List.rev_append (read_prover_events db_path) (read_checker_events db_path)

let read_meta db_path : Test_metadata.t =
  let meta_rows = run_query db_path "SELECT key,value FROM meta" in
  let meta_map =
    List.fold_left
      (fun m row -> Misc.Str_map.add (str row "key") (str row "value") m)
      Misc.Str_map.empty meta_rows
  in
  let uuid =
    match Misc.Str_map.find_opt "uuid" meta_map with
    | Some s ->
      (match Uuidm.of_string s with
      | Some u -> u
      | None -> Uuidm.nil)
    | None -> Uuidm.nil
  in
  let timestamp =
    match Misc.Str_map.find_opt "timestamp" meta_map with
    | Some s -> (try Some (float_of_string s) with _ -> None)
    | None -> None
  in
  let total_wall_time =
    match Misc.Str_map.find_opt "total-wall-time" meta_map with
    | Some s -> (try Some (float_of_string s) with _ -> None)
    | None -> None
  in
  let n_results =
    match run_query db_path "SELECT count(*) AS cnt FROM prover_res" with
    | [ row ] -> int_ row "cnt"
    | _ -> 0
  in
  let provers =
    run_query db_path "SELECT DISTINCT prover FROM prover_res"
    |> List.map (fun row -> str row "prover")
  in
  {
    Test_metadata.uuid;
    timestamp;
    total_wall_time;
    n_results;
    n_bad = 0;
    dirs = [];
    provers;
  }

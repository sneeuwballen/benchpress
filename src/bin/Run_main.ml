open Common
module T = Test
module Log = (val Logs.src_log (Logs.Src.create "benchpress.run-main"))
module Api = Benchpress_api_proto.Benchpress_api

[@@@alert "-unstable"]

(* ── NATS progress publishing ───────────────────────────────────────────── *)

(** Create a [Progress.callbacks] that publishes each report to NATS. *)
let make_nats_progress_cb ~(nats : Nats.t) ~uuid : Progress.callbacks =
  let uuid_s = Uuidm.to_string uuid in
  let solve_bp_subject = [ "benchpress"; "progress"; "solve"; uuid_s ] in
  let check_bp_subject = [ "benchpress"; "progress"; "check"; uuid_s ] in
  let done_bp_subject = [ "benchpress"; "progress"; "done"; uuid_s ] in
  let solve_user_subject =
    [ "user"; "notify"; "benchpress"; "solve"; uuid_s ]
  in
  let check_user_subject =
    [ "user"; "notify"; "benchpress"; "check"; uuid_s ]
  in
  let done_user_subject = [ "user"; "notify"; "benchpress"; "done"; uuid_s ] in
  let last_send = ref 0.0 in
  let last_user_notify = ref 0.0 in
  let sent_done = ref false in
  let publish kind report =
    let now = Unix.gettimeofday () in
    let finished = report.Api.finished in
    let should_send =
      ((not finished) && now -. !last_send >= 1.0)
      || (finished && not !sent_done)
    in
    if should_send then (
      last_send := now;
      if finished then sent_done := true;
      let bp_subject, user_subject =
        match kind with
        | `Solve -> solve_bp_subject, solve_user_subject
        | `Check -> check_bp_subject, check_user_subject
        | `Done -> done_bp_subject, done_user_subject
      in
      let json =
        Api.encode_json_progress_report report |> Yojson.Basic.to_string
      in
      (try Nats.pub nats ~subject:bp_subject json with _ -> ());
      let should_user_notify =
        (* first notification, done notification, or ~15 min since last *)
        !last_user_notify = 0.0 || finished || now -. !last_user_notify >= 900.0
      in
      if should_user_notify then (
        last_user_notify := now;
        try Nats.pub nats ~subject:user_subject json with _ -> ()
      )
    )
  in
  let make_done_report () =
    let r = Api.default_progress_report () in
    Api.progress_report_set_uuid r uuid_s;
    Api.progress_report_set_start_ts r 0.;
    Api.progress_report_set_finished r true;
    r
  in
  {
    Progress.on_report = (fun r -> publish `Solve r);
    on_done = (fun () -> publish `Done (make_done_report ()));
    on_res = (fun _ -> ());
  }

(* run provers on the given dirs, return a list [prover, dir, results] *)
let execute_run_prover_action ?j ?cpus ?timestamp ?pp_results ?dyn ?limits
    ?proof_dir ?output ~notify ~uuid ~save ~wal_mode ~update ?(compress = false)
    ?progress_cb ~total_tasks_ref (defs : Definitions.t)
    (r : Action.run_provers) : Test_top_result.t lazy_t * Test_compact_result.t
    =
  let@ () =
    Error.guard
      (Error.wrapf "run prover action@ `@[%a@]`" Action.pp_run_provers r)
  in
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "run-prover" in
  let interrupted = Atomic.make false in
  let r =
    Exec_action.Exec_run_provers.expand ?dyn ?j ?cpus ?proof_dir ?limits defs
      r.limits r.j r.pattern r.dirs r.provers
  in
  let len = List.length r.problems in
  let total_tasks = len * List.length r.provers in
  total_tasks_ref := total_tasks;
  Notify.sendf notify "testing with %d provers, %d problems…"
    (List.length r.provers) len;
  let uuid_s = Uuidm.to_string uuid in
  let start_ts = CCOpt.value ~default:(Misc.now_s ()) timestamp in
  let progress_components =
    (if CCOpt.get_or ~default:false dyn then
       [ Progress.make_terminal ~total_tasks ?pp_results () ]
     else
       [])
    @
    match progress_cb with
    | None -> []
    | Some cb ->
      [ Progress.make ~uuid:uuid_s ~start_ts ~total_tasks ~callbacks:cb ]
  in
  let progress : Progress.t =
    match progress_components with
    | [] -> Progress.nil
    | [ t ] -> t
    | l -> Progress.fanout l
  in
  (* solve *)
  let result =
    Error.guard (Error.wrapf "running %d tests" len) @@ fun () ->
    Exec_action.Exec_run_provers.run ~uuid ?timestamp
      ~interrupted:(fun () -> Atomic.get interrupted)
      ~on_solve:progress#on_res ~save ~wal_mode ~compress ?output ~update
      ~on_start_proof_check:(fun () -> progress#on_start_proof_check)
      ~on_proof_check:progress#on_proof_check_res
      ~on_done:(fun _ -> progress#on_done)
      r
  in
  result

let execute_submit_job_action ?j ?timestamp ?dyn ?limits ?proof_dir ?output
    ~notify ~(uuid : Uuidm.t) ~(save : bool) ~wal_mode ~update
    ?(compress = false) ?progress_cb (defs : Definitions.t)
    (r : Action.run_provers_slurm_submission) : _ * Test_compact_result.t =
  let@ _sp =
    Error.guard
      (Error.wrapf "run provers with slurm action@ `@[%a@]`"
         Action.pp_run_provers_slurm r)
  in
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "run-submit-job-action" in
  let interrupted = Atomic.make false in
  let exp_r =
    Exec_action.Exec_run_provers.expand ~slurm:true ?dyn ?j ?proof_dir ?limits
      defs r.limits r.j r.pattern r.dirs r.provers
  in
  let len = List.length exp_r.problems in
  let total_tasks = len * List.length r.provers in
  Notify.sendf notify "testing with %d provers, %d problems…"
    (List.length r.provers) len;
  let uuid_s = Uuidm.to_string uuid in
  let start_ts = Misc.now_s () in
  let progress_components =
    (if CCOpt.get_or ~default:false dyn then
       [ Progress.make_terminal ~total_tasks () ]
     else
       [])
    @
    match progress_cb with
    | None -> []
    | Some cb ->
      [ Progress.make ~uuid:uuid_s ~start_ts ~total_tasks ~callbacks:cb ]
  in
  let progress =
    match progress_components with
    | [] -> Progress.nil
    | [ t ] -> t
    | l -> Progress.fanout l
  in
  (* submit job *)
  let result =
    Error.guard (Error.wrapf "running %d tests" len) @@ fun () ->
    Exec_action.Exec_run_provers.run_sbatch_job ~uuid ?timestamp
      ~interrupted:(fun () -> Atomic.get interrupted)
      ?partition:r.partition ~nodes:r.nodes ~addr:r.addr ~port:r.port
      ~ntasks:r.ntasks ~save ~wal_mode ~compress ?output ~update
      ~on_solve:progress#on_res
      ~on_start_proof_check:(fun () -> progress#on_start_proof_check)
      ~on_proof_check:progress#on_proof_check_res
      ~on_done:(fun _ -> progress#on_done)
      exp_r
  in
  result

type top_task =
  | TT_run_provers of Action.run_provers * Definitions.t
  | TT_other of Action.t
  | TT_run_slurm_submission of
      Action.run_provers_slurm_submission * Definitions.t

let main ?j ?cpus ?pp_results ?dyn ?timeout ?memory ?csv ?(provers = []) ?meta:_
    ?summary ?task ?(dir_files = []) ?proof_dir ?output ?(save = true)
    ?(wal_mode = false) ?(compress = false) ~desktop_notification ~no_failure
    ~update ?(sbatch = false) ?partition ?nodes ?addr ?port ?ntasks ?nats
    (defs : Definitions.t) paths () : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "main" in
  Log.info (fun k ->
      k "run-main.main for paths %a" (Misc.pp_list Misc.Pp.pp_str) paths);
  Log.debug (fun k -> k "definitions: %a" Definitions.pp defs);
  let timestamp = Unix.gettimeofday () in
  let notify = Notify.make defs in
  (* parse list of files, if need be *)
  let paths = Definitions.mk_paths ~dir_files paths in
  (* parse config *)
  let tt_task =
    match task with
    | Some task_name ->
      let t = Definitions.find_task defs task_name in
      (match t with
      | { view = { Task.action = Action.Act_run_provers r; _ }; loc }
        when sbatch ->
        Log.warn (fun k ->
            k "Calling benchpress in slurm mode on a normal run task!");
        Error.guard (Error.wrap ~loc "running task 'run provers with slurm'")
        @@ fun () ->
        let j = CCOpt.( <+> ) j r.j in
        let timeout =
          CCOpt.( <+> ) timeout
            (CCOpt.map_or ~default:None
               (fun t -> Some (Limit.Time.as_int Seconds t))
               r.limits.time)
        in
        let memory =
          CCOpt.( <+> ) memory
            (CCOpt.map_or ~default:None
               (fun t -> Some (Limit.Memory.as_int Megabytes t))
               r.limits.memory)
        in
        let r =
          Definitions.mk_run_provers_slurm_submission ?partition ?nodes ?j ?addr
            ?port ?ntasks ~paths ?timeout ?memory ~provers ?loc:r.loc defs
        in
        let r =
          {
            r with
            provers =
              r.provers @ r.provers
              |> CCList.sort_uniq ~cmp:Prover.compare_by_name;
            dirs = r.dirs @ r.dirs;
          }
        in
        TT_run_slurm_submission (r, defs)
      | { view = { Task.action = Action.Act_run_provers r; _ }; loc } ->
        Error.guard (Error.wrap ~loc "running task 'run provers'") @@ fun () ->
        (* convert paths and provers *)
        let paths = CCList.map (Definitions.mk_subdir defs) paths in
        let provers = CCList.map (Definitions.find_prover' defs) provers in
        let provers =
          provers @ r.provers |> CCList.sort_uniq ~cmp:Prover.compare_by_name
        in
        let r = { r with provers; dirs = paths @ r.dirs } in
        TT_run_provers (r, defs)
      | { view = { Task.action = Action.Act_run_slurm_submission r; _ }; loc }
        ->
        Error.guard (Error.wrap ~loc "running task 'run provers with slurm'")
        @@ fun () ->
        let r =
          {
            r with
            nodes = CCOpt.value ~default:r.nodes nodes;
            j = CCOpt.( <+> ) j r.j;
            limits =
              {
                r.limits with
                time =
                  CCOpt.map_or ~default:r.limits.time
                    (fun s -> Some (Limit.Time.mk ~s ()))
                    timeout;
                memory =
                  CCOpt.map_or ~default:r.limits.memory
                    (fun m -> Some (Limit.Memory.mk ~m ()))
                    memory;
              };
            provers =
              CCList.map (Definitions.find_prover' defs) provers @ r.provers
              |> CCList.sort_uniq ~cmp:Prover.compare_by_name;
            dirs = CCList.map (Definitions.mk_subdir defs) paths @ r.dirs;
          }
        in
        TT_run_slurm_submission (r, defs)
      | { loc = _; view = t } -> TT_other t.action)
    | None when sbatch ->
      let r : Action.run_provers_slurm_submission =
        Definitions.mk_run_provers_slurm_submission ?partition ?nodes ?j ?addr
          ?port ?ntasks ~paths ?timeout ?memory ~provers defs
      in
      TT_run_slurm_submission (r, defs)
    | None ->
      let provers =
        match provers with
        | [] -> Error.fail "please provide at least one prover"
        | l -> l
      in
      let provers = CCList.sort_uniq ~cmp:Prover.compare_name provers in
      (* deduplicate *)
      let r =
        Definitions.mk_run_provers ~loc:None ?timeout ?memory ?j ~provers ~paths
          defs
      in
      TT_run_provers (r, defs)
  in
  match tt_task with
  | TT_other a -> Exec_action.run ?output ~save defs a
  | TT_run_provers (run_provers_action, defs) ->
    let j = CCOpt.Infix.(j <+> Definitions.option_j defs) in
    let progress = CCOpt.Infix.(dyn <+> Definitions.option_progress defs) in
    let limits = run_provers_action.limits in
    let uuid = Misc.mk_uuid () in
    let total_tasks_ref = ref 0 in
    let server_progress_cb =
      match nats with
      | Some nats -> Some (make_nats_progress_cb ~nats ~uuid)
      | None -> None
    in
    let top_res, (results : Test_compact_result.t) =
      execute_run_prover_action ~uuid ?pp_results ?proof_dir ?dyn:progress
        ~limits ?j ?cpus ?output ~notify ~timestamp ~save ~wal_mode ~update
        ~compress ?progress_cb:server_progress_cb ~total_tasks_ref defs
        run_provers_action
    in
    if CCOpt.is_some csv then (
      let res = Lazy.force top_res in
      Bin_utils.dump_csv ~csv res
    );
    if CCOpt.is_some summary then (
      let res = Lazy.force top_res in
      Bin_utils.dump_summary ~summary res
    );
    let r = Bin_utils.check_compact_res ~no_failure notify results in
    Notify.sync notify;
    Bin_utils.printbox_compact_results results;
    (* try to send a desktop notification *)
    if desktop_notification then (
      try
        CCUnix.call "notify-send 'benchmark done (%s)'"
          (CCOpt.map_or ~default:"?" Misc.human_duration
             results.cr_meta.total_wall_time)
        |> ignore
      with _ -> ()
    );
    r
  | TT_run_slurm_submission (run_provers_action_sbatch, defs) ->
    let j = CCOpt.Infix.(j <+> Definitions.option_j defs) in
    let progress = CCOpt.Infix.(dyn <+> Definitions.option_progress defs) in
    let limits = run_provers_action_sbatch.limits in
    let uuid = Misc.mk_uuid () in

    let top_res, (results : Test_compact_result.t) =
      execute_submit_job_action ~uuid ?proof_dir ?dyn:progress ~limits ?j
        ?output ~notify ~timestamp ~save ~wal_mode ~update ~compress defs
        run_provers_action_sbatch
    in
    if CCOpt.is_some csv then (
      let res = Lazy.force top_res in
      Bin_utils.dump_csv ~csv res
    );
    if CCOpt.is_some summary then (
      let res = Lazy.force top_res in
      Bin_utils.dump_summary ~summary res
    );
    let r = Bin_utils.check_compact_res ~no_failure notify results in
    Notify.sync notify;
    Bin_utils.printbox_compact_results results;
    (* try to send a desktop notification *)
    if desktop_notification then (
      try
        CCUnix.call "notify-send 'benchmark done (%s)'"
          (CCOpt.map_or ~default:"?" Misc.human_duration
             results.cr_meta.total_wall_time)
        |> ignore
      with _ -> ()
    );
    r

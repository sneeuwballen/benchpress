open Common
module T = Test

module Log = (val Logs.src_log (Logs.Src.create "benchpress.run-main"))

(* run provers on the given dirs, return a list [prover, dir, results] *)
let execute_run_prover_action
    ?j ?timestamp ?pp_results ?dyn ?limits ?proof_dir ~notify ~uuid ~save
    (defs: Definitions.t) (r:Action.run_provers)
  : (_ * Test_compact_result.t) =
  begin
    Error.guard (Error.wrapf "run prover action@ `@[%a@]`" Action.pp_run_provers r) @@ fun () ->
    let interrupted = CCLock.create false in
    let r = Exec_action.Exec_run_provers.expand ?dyn ?j ?proof_dir ?limits defs r in
    let len = List.length r.problems in
    Notify.sendf notify "testing with %d provers, %d problems…"
      (List.length r.provers) len;
    let progress =
      Exec_action.Progress_run_provers.make ?pp_results ?dyn r in
    (* solve *)
    let result =
      Error.guard (Error.wrapf "running %d tests" len) @@ fun () ->
      Exec_action.Exec_run_provers.run ~uuid ?timestamp
        ~interrupted:(fun () -> CCLock.get interrupted)
        ~on_solve:progress#on_res ~save ~on_done:(fun _ -> progress#on_done) r
    in
    result
  end

type top_task =
  | TT_run_provers of Action.run_provers * Definitions.t
  | TT_other of Action.t

let main ?j ?pp_results ?dyn ?timeout ?memory ?csv ?(provers=[])
    ?meta:_ ?summary ?task ?dir_file ?proof_dir ?(save=true)
    (defs:Definitions.t) paths () : unit =
  Log.info
    (fun k->k"run-main.main for paths %a" (Misc.pp_list Misc.Pp.pp_str) paths);
  let timestamp = Unix.gettimeofday() in
  let notify = Notify.make defs in
  (* parse list of files, if need be *)
  let paths = match dir_file with
    | None -> paths
    | Some f ->
      let f_lines = CCIO.with_in f CCIO.read_lines_l in
      List.rev_append f_lines paths
  in
  (* parse config *)
  let tt_task =
    match task with
    | Some task_name ->
      let t = Definitions.find_task defs task_name in
      begin match t with
        | {view={Task.action=Action.Act_run_provers r;_};loc} ->
          Error.guard (Error.wrap ~loc "running task 'run provers'") @@ fun () ->
          (* convert paths and provers *)
          let paths = CCList.map (Definitions.mk_subdir defs) paths in
          let provers = CCList.map (Definitions.find_prover' defs) provers in
          let provers =
            provers @ r.provers
            |> CCList.sort_uniq ~cmp:Prover.compare_by_name
          in
          let r = {r with provers; dirs=paths @ r.dirs} in
          TT_run_provers (r,defs)

        | {loc=_;view=t} ->
          TT_other t.action
      end
    | None ->
      let provers =
        match provers with
       | [] -> Error.fail "please provide at least one prover"
       | l -> l
      in
      let provers = CCList.sort_uniq ~cmp:Prover.compare_name provers in (* deduplicate *)
      let r =
        Definitions.mk_run_provers ~loc:None ?timeout ?memory ?j ~provers ~paths defs in
      TT_run_provers (r, defs)
  in
  begin match tt_task with
    | TT_other a ->
      Exec_action.run ~save defs a
    | TT_run_provers (run_provers_action, defs) ->
      let j = CCOpt.Infix.( j <+> Definitions.option_j defs) in
      let progress = CCOpt.Infix.( dyn <+> Definitions.option_progress defs) in
      let limits = run_provers_action.limits in
      (* run action here! *)
      let uuid = Misc.mk_uuid() in

      let (top_res, (results:Test_compact_result.t)) =
        execute_run_prover_action
          ~uuid ?pp_results ?proof_dir ?dyn:progress ~limits ?j ~notify ~timestamp ~save
          defs run_provers_action
      in
      if CCOpt.is_some csv then (
        let res = Lazy.force top_res in
        Bin_utils.dump_csv ~csv res;
      );
      if CCOpt.is_some summary then (
        let res = Lazy.force top_res in
        Bin_utils.dump_summary ~summary res
      );
      (* now fail if results were bad *)
      let r = Bin_utils.check_compact_res notify results in
      Notify.sync notify;
      Bin_utils.printbox_compact_results results;
      (* try to send a desktop notification *)
      (try CCUnix.call "notify-send 'benchmark done (%s)'"
             (CCOpt.map_or ~default:"?" Misc.human_duration
                results.cr_meta.total_wall_time) |> ignore
       with _ -> ());
      r
  end

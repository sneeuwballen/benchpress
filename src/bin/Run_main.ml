module T = Test
module E = Or_error
open E.Infix
type 'a or_error = 'a Or_error.t

module Log = (val Logs.src_log (Logs.Src.create "benchpress.run-main"))

(* run provers on the given dirs, return a list [prover, dir, results] *)
let execute_run_prover_action
    ?j ?timestamp ?pp_results ?dyn ?limits ~notify ~uuid ~save
    (r:Action.run_provers)
  : (_ * Test_compact_result.t) or_error =
  begin
    let interrupted = CCLock.create false in
    Exec_action.Exec_run_provers.expand ?dyn ?j ?limits r >>= fun r ->
    let len = List.length r.problems in
    Notify.sendf notify "testing with %d provers, %d problems…"
      (List.length r.provers) len;
    let progress =
      Exec_action.Progress_run_provers.make ?pp_results ?dyn r in
    (* solve *)
    let* result =
      Exec_action.Exec_run_provers.run ~uuid ?timestamp
        ~interrupted:(fun () -> CCLock.get interrupted)
        ~on_solve:progress#on_res ~save ~on_done:(fun _ -> progress#on_done) r
      |> E.wrapf "running %d tests" len
    in
    E.return result
  end |> E.wrap "running tests"

type top_task =
  | TT_run_provers of Action.run_provers
  | TT_other of Action.t

let main ?j ?pp_results ?dyn ?timeout ?memory ?csv ?(provers=[])
    ?meta:_ ?summary ?task ?dir_file ?(save=true)
    (defs:Definitions.t) paths () : unit or_error =
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
  begin match task with
    | Some task_name ->
      begin Definitions.find_task defs task_name >>= function
        | {view={Task.action=Action.Act_run_provers r;_};loc} ->
          (* convert paths and provers *)
          begin
            E.map_l (Definitions.mk_subdir defs) paths >>= fun paths ->
            E.map_l (Definitions.find_prover' defs) provers >>= fun provers ->
            let provers =
              provers @ r.provers
              |> CCList.sort_uniq ~cmp:Prover.compare_by_name
            in
            let r = {r with provers; dirs=paths @ r.dirs} in
            Ok (TT_run_provers r)
          end |> E.wrap ~loc "running task 'run provers'"
        | {loc=_;view=t} ->
          Ok (TT_other t.action)
      end
    | None ->
      (match provers with
       | [] -> E.fail "please provide at least one prover"
       | l -> Ok l
      ) >>= fun provers ->
      let provers = CCList.sort_uniq ~cmp:Prover.compare_name provers in (* deduplicate *)
      Definitions.mk_run_provers ~loc:None ?timeout ?memory ?j ~provers ~paths defs >>= fun r ->
      Ok (TT_run_provers r)
  end >>= fun tt_task ->
  begin match tt_task with
    | TT_other a ->
      Exec_action.run ~save defs a
    | TT_run_provers run_provers_action ->
      let j = CCOpt.Infix.( j <+> Definitions.option_j defs) in
      let progress = CCOpt.Infix.( dyn <+> Definitions.option_progress defs) in
      let limits = run_provers_action.limits in
      (* run action here! *)
      let uuid = Misc.mk_uuid() in
      execute_run_prover_action
        ~uuid ?pp_results ?dyn:progress ~limits ?j ~notify ~timestamp ~save
        run_provers_action
      >>= fun (top_res, (results:Test_compact_result.t)) ->
      let*() = if CCOpt.is_some csv then (
          let+ res = Lazy.force top_res in
          Bin_utils.dump_csv ~csv res;
        ) else E.return ()
      in
      let* () =
        if CCOpt.is_some summary then (
          let+ res = Lazy.force top_res in
          Bin_utils.dump_summary ~summary res
        ) else E.return()
      in
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

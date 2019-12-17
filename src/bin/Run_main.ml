module T = Test
module E = CCResult
type 'a or_error = ('a, string) E.t

(* run provers on the given dirs, return a list [prover, dir, results] *)
let execute_run_prover_action
    ?j ?timestamp ?dyn ?timeout ?memory ~notify
    (r:Action.run_provers)
  : T.Top_result.t or_error =
  let open E.Infix in
  begin
    Exec_action.Exec_run_provers.expand ?j ?timeout ?memory r >>= fun r ->
    let len = List.length r.problems in
    Notify.sendf notify "testing with %d provers, %d problems…"
      (List.length r.provers) len;
    let progress = Exec_action.Progress_run_provers.make ?dyn r in
    (* solve *)
    begin
      Exec_action.Exec_run_provers.run ?timestamp ~on_solve:progress r
      |> E.add_ctxf "running %d tests" len
    end
    >>= fun results ->
    let analyze = T.Top_result.analyze results in
    List.iter
      (fun (p,r) ->
         Misc.synchronized (fun () ->
         Format.printf "(@[<2>:prover %s @[<2>:results@ %a@]@])@."
           p T.Analyze.pp r))
      analyze;
    E.return results
  end |> E.add_ctxf "running tests"

let main ?j ?dyn ?timeout ?memory ?csv ?provers
    ?meta:_ ?summary ?task ?dir_file (defs:Definitions.t) paths () =
  let open E.Infix in
  Logs.info
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
        | {Task.action=Action.Act_run_provers r;_} ->
          (* convert paths and provers *)
          E.map_l (Definitions.mk_subdir defs) paths >>= fun paths ->
          E.map_l (Definitions.find_prover defs)
            (CCOpt.get_or ~default:[] provers) >>= fun provers ->
          let r = {r with provers=provers @ r.provers; dirs=paths @ r.dirs} in
          (* TODO: more general framework for running and reporting actions *)
          Ok r
          (* | _ -> E.fail_fprintf "cannot run task %a yet" Task.pp t *)
      end
    | None ->
      (match provers with
       | None | Some [] -> E.fail_fprintf "please provide at least one prover"
       | Some l -> Ok l
      ) >>= fun provers ->
      Definitions.mk_run_provers ?timeout ?memory ?j ~provers ~paths defs
  end >>= fun run_provers_action ->
  let j = CCOpt.Infix.( j <+> Definitions.option_j defs) in
  let progress = CCOpt.Infix.( dyn <+> Definitions.option_progress defs) in
  execute_run_prover_action ?dyn:progress ?timeout ?memory ?j ~notify ~timestamp
    run_provers_action
  >>= fun (results:T.Top_result.t) ->
  Utils.dump_csv ~csv results;
  Utils.dump_summary ~summary results;
  Utils.dump_results_sqlite results;
  (* now fail if results were bad *)
  let r = Utils.check_res notify results in
  Notify.sync notify;
  Utils.printbox_results results;
  (* try to send a desktop notification *)
  (try CCUnix.call "notify-send 'benchmark done (%s)'"
         (Misc.human_time results.T.total_wall_time) |> ignore
   with _ -> ());
  r

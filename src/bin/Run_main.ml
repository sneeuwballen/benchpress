module T = Test
module E = CCResult
type 'a or_error = ('a, string) E.t

(* run provers on the given dirs, return a list [prover, dir, results] *)
let execute_run_prover_action
    ?j ?timestamp ?dyn ?timeout ?memory ~notify ~uuid
    (r:Action.run_provers)
  : (_ * T.Compact_result.t) or_error =
  let open E.Infix in
  begin
    Exec_action.Exec_run_provers.expand ?dyn ?j ?timeout ?memory r >>= fun r ->
    let len = List.length r.problems in
    Notify.sendf notify "testing with %d provers, %d problems…"
      (List.length r.provers) len;
    let progress = Exec_action.Progress_run_provers.make ?dyn r in
    (* solve *)
    begin
      Exec_action.Exec_run_provers.run ~uuid ?timestamp ~on_solve:progress r
      |> E.add_ctxf "running %d tests" len
    end
    >>= fun results ->
    E.return results
  end |> E.add_ctxf "running tests"

type top_task =
  | TT_run_provers of Action.run_provers
  | TT_other of Action.t

let main ?j ?dyn ?timeout ?memory ?csv ?(provers=[])
    ?meta:_ ?summary ?task ?dir_file (defs:Definitions.t) paths () : unit or_error =
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
          E.map_l (Definitions.find_prover defs) provers >>= fun provers ->
          let r = {r with provers=provers @ r.provers; dirs=paths @ r.dirs} in
          Ok (TT_run_provers r)
        | t ->
          Ok (TT_other t.action)
      end
    | None ->
      (match provers with
       | [] -> E.fail_fprintf "please provide at least one prover"
       | l -> Ok l
      ) >>= fun provers ->
      Definitions.mk_run_provers ?timeout ?memory ?j ~provers ~paths defs >>= fun r ->
      Ok (TT_run_provers r)
  end >>= fun tt_task ->
  begin match tt_task with
    | TT_other a ->
      Exec_action.run defs a
    | TT_run_provers run_provers_action ->
      let j = CCOpt.Infix.( j <+> Definitions.option_j defs) in
      let progress = CCOpt.Infix.( dyn <+> Definitions.option_progress defs) in
      (* run action here! *)
      let uuid = Misc.mk_uuid() in
      execute_run_prover_action ~uuid ?dyn:progress ?timeout ?memory ?j ~notify ~timestamp
        run_provers_action
      >>= fun (top_res, (results:T.Compact_result.t)) ->
      if CCOpt.is_some csv then (
        Utils.dump_csv ~csv @@ Lazy.force top_res;
      );
      if CCOpt.is_some summary then (
        Utils.dump_summary ~summary @@ Lazy.force top_res;
      );
      (* now fail if results were bad *)
      let r = Utils.check_compact_res notify results in
      Notify.sync notify;
      Utils.printbox_compact_results results;
      (* try to send a desktop notification *)
      (try CCUnix.call "notify-send 'benchmark done (%s)'"
             (CCOpt.map_or ~default:"?" Misc.human_duration results.T.cr_meta.total_wall_time) |> ignore
       with _ -> ());
      r
  end

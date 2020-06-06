module T = Test
module E = CCResult
type 'a or_error = ('a, string) E.t

let mk_progress_api ~uuid api_port : _ option =
  match api_port with
  | None -> None
  | Some p ->
    let task = {
      Api.t_id=Uuidm.to_string uuid;
      t_descr="run";
      t_status=Api.T_waiting;
    } in
    let send_ q =
      let ic, oc =
        Unix.open_connection (Unix.ADDR_INET(Unix.inet_addr_loopback, p))
      in
      try
        CCFun.finally
          ~h:(fun () -> close_in_noerr ic; close_out_noerr oc)
          ~f:(fun () ->
            Printf.fprintf oc "%s\n%!" (Api.pb_to_string Api.encode_query q))
      with e ->
        Logs.err (fun k->k "could not connect to API: %s" (Printexc.to_string e))
    in
    let r = object
      method on_progress ~percent ~elapsed_time:t ~eta:_ =
        let q =
          Api.Q_task_update {
            task with
            t_status=Api.T_in_progress {
                Api.estimated_completion=Int32.of_int percent; time_elapsed=t;
              };
          }
        in
        send_ q

      method on_done =
        let q = Api.Q_task_update { task with t_status=Api.T_done } in
        send_ q
    end
    in
    Some r

(* run provers on the given dirs, return a list [prover, dir, results] *)
let execute_run_prover_action
    ?api_port ?j ?timestamp ?pp_results ?dyn ?timeout ?memory ~notify ~uuid
    (r:Action.run_provers)
  : (_ * T.Compact_result.t) or_error =
  let open E.Infix in
  begin
    let cb_progress = mk_progress_api ~uuid api_port in
    Exec_action.Exec_run_provers.expand ?dyn ?j ?timeout ?memory r >>= fun r ->
    let len = List.length r.problems in
    Notify.sendf notify "testing with %d provers, %d problems…"
      (List.length r.provers) len;
    let progress = Exec_action.Progress_run_provers.make ?cb_progress ?pp_results ?dyn r in
    (* solve *)
    begin
      Exec_action.Exec_run_provers.run ~uuid ?timestamp
        ~on_solve:progress#on_res ~on_done:(fun _ -> progress#on_done) r
      |> E.add_ctxf "running %d tests" len
    end
    >>= fun results ->
    E.return results
  end |> E.add_ctxf "running tests"

type top_task =
  | TT_run_provers of Action.run_provers
  | TT_other of Action.t

let main ?j ?pp_results ?dyn ?timeout ?memory ?csv ?(provers=[])
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
          let provers =
            provers @ r.provers
            |> CCList.sort_uniq ~cmp:Prover.compare_by_name
          in
          let r = {r with provers; dirs=paths @ r.dirs} in
          Ok (TT_run_provers r)
        | t ->
          Ok (TT_other t.action)
      end
    | None ->
      (match provers with
       | [] -> E.fail_fprintf "please provide at least one prover"
       | l -> Ok l
      ) >>= fun provers ->
      let provers = CCList.sort_uniq ~cmp:Prover.compare_name provers in (* deduplicate *)
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
      execute_run_prover_action
        ~api_port:Api.default_port
        ~uuid ?pp_results ?dyn:progress ?timeout ?memory ?j ~notify ~timestamp
        run_provers_action
      >>= fun (top_res, (results:T.Compact_result.t)) ->
      if CCOpt.is_some csv then (
        Bin_utils.dump_csv ~csv @@ Lazy.force top_res;
      );
      if CCOpt.is_some summary then (
        Bin_utils.dump_summary ~summary @@ Lazy.force top_res;
      );
      (* now fail if results were bad *)
      let r = Bin_utils.check_compact_res notify results in
      Notify.sync notify;
      Bin_utils.printbox_compact_results results;
      (* try to send a desktop notification *)
      (try CCUnix.call "notify-send 'benchmark done (%s)'"
             (CCOpt.map_or ~default:"?" Misc.human_duration results.T.cr_meta.total_wall_time) |> ignore
       with _ -> ());
      r
  end

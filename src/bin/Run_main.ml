module T = Test
module E = CCResult
type 'a or_error = ('a, string) E.t

let mk_progress_api ?interrupted ~uuid api_port : _ option =
  let module M = CCLock in
  match api_port with
  | None -> None
  | Some p ->
    let task = {
      Api.t_id=Uuidm.to_string uuid;
      t_descr="run provers";
      t_status=Api.T_waiting;
    } in
    let period = 0.3 in
    let bad_tries = ref 0 in (* give up after a while *)
    let state_and_done = M.create (task,false) in
    (* separate thread to update the server *)
    let rec loop_send() =
      let tsk, is_done = M.get state_and_done in
      if (period *. float !bad_tries) > 60. then (
        Logs.info (fun k->k"too many errors, give up on the API");
      ) else if not is_done then (
        Logs.debug (fun k->k"send progress api to the server");
        begin
          try
            let ic, oc =
              Unix.open_connection (Unix.ADDR_INET(Unix.inet_addr_loopback, p))
            in
            CCFun.finally
              ~h:(fun () -> close_in_noerr ic; close_out_noerr oc)
              ~f:(fun () ->
                  let q = Api.Q_task_update tsk in
                  Printf.fprintf oc "%s\n%!" (Api.pb_to_string Api.encode_query q);
                  close_out_noerr oc;
                  let r = CCIO.read_all ic in
                  match Api.pb_of_string Api.decode_response r with
                  | Api.R_error e -> failwith e
                  | Api.R_ok -> ()
                  | Api.R_interrupted ->
                    Logs.warn (fun k->k"interrupted by API");
                    CCOpt.iter (fun m -> CCLock.set m true) interrupted;
                );
              bad_tries := 0
          with e ->
            Logs.warn
              (fun k->k "error when connecting to API: %s" (Printexc.to_string e));
            incr bad_tries;
        end;
        Thread.delay period;
        loop_send()
      )
    in
    let _th_sender = Thread.create loop_send () in
    let r = object
      method on_progress ~percent ~elapsed_time:t ~eta:_ =
        M.update state_and_done
          (fun (task,_) ->
            {task with
              t_status=Api.T_in_progress {
              Api.estimated_completion=Int32.of_int percent; time_elapsed=t;
            }},false)

      method on_done =
        M.set state_and_done ({ task with t_status=Api.T_done }, true) ;
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
    let interrupted = CCLock.create false in
    let cb_progress = mk_progress_api ~interrupted ~uuid api_port in
    Exec_action.Exec_run_provers.expand ?dyn ?j ?timeout ?memory r >>= fun r ->
    let len = List.length r.problems in
    Notify.sendf notify "testing with %d provers, %d problems…"
      (List.length r.provers) len;
    let progress =
      Exec_action.Progress_run_provers.make ?cb_progress ?pp_results ?dyn r in
    (* solve *)
    begin
      Exec_action.Exec_run_provers.run ~uuid ?timestamp
        ~interrupted:(fun () -> CCLock.get interrupted)
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
             (CCOpt.map_or ~default:"?" Misc.human_duration
                results.T.cr_meta.total_wall_time) |> ignore
       with _ -> ());
      r
  end

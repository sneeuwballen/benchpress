module Log = (val Logs.src_log (Logs.Src.create "benchpress-worker"))

let spf = Format.sprintf

let copy_problem ~proof_dir ~prover (file : string) : unit =
  let basename = spf "pb-%s-%s" prover.Prover.name (Filename.basename file) in
  let new_path = Filename.concat proof_dir basename in
  Log.debug (fun k -> k "(@[copy-problem@ :from %S@ :to %S@])" file new_path);
  CCIO.with_out new_path @@ fun oc ->
  CCIO.with_in file @@ fun ic -> CCIO.copy_into ~bufsize:(64 * 1024) ic oc

let run_prover_pb ?proof_dir ~limits ~prover ~pb provers checkers =
  let prover =
    try With_loc.view (Misc.Str_map.find prover provers)
    with Not_found -> Error.failf "cannot find prover '%s'" prover
  in
  Error.guard
    (Error.wrapf "(@[worker: running :prover %a :on %a@])" Prover.pp_name prover
       Problem.pp pb)
  @@ fun () ->
  let proof_file, keep =
    if prover.Prover.produces_proof then (
      let pb = pb.Problem.name in
      let ext = CCOption.get_or ~default:"proof" prover.Prover.proof_ext in
      let basename =
        spf "proof-%s-%s.%s" prover.Prover.name (Filename.basename pb) ext
      in
      let filename, keep =
        match proof_dir with
        | None -> Filename.concat (Filename.dirname pb) basename, false
        | Some dir ->
          copy_problem ~proof_dir:dir ~prover pb;
          Filename.concat dir basename, true
      in
      Some filename, keep
    ) else
      None, false
  in
  let result = Run_prover_problem.run ~limits ~proof_file prover pb in
  (* Determine check requests via get_checkers. *)
  let check_reqs =
    match prover.Prover.get_checkers with
    | None -> []
    | Some get_checkers ->
      get_checkers ~stdout:result.raw.Run_proc_result.stdout
        ~stderr:result.raw.Run_proc_result.stderr ~res:result.res ~proof_file
      |> CCList.filter_map (fun (checker_name, pf_override) ->
             let actual_pf =
               match pf_override with
               | Some p -> p
               | None ->
                 (match proof_file with
                 | Some p -> p
                 | None ->
                   Error.failf
                     "prover %s: get_checkers returned checker %s but no proof \
                      file available"
                     prover.Prover.name checker_name)
             in
             match Misc.Str_map.find_opt checker_name checkers with
             | None ->
               Log.warn (fun k -> k "worker: unknown checker '%s'" checker_name);
               None
             | Some checker -> Some (With_loc.view checker, actual_pf))
  in
  (* Inline proof checking (worker does not have a dynamic queue). *)
  let ev_proofs =
    List.map
      (fun (checker, pfile) ->
        Log.debug (fun k ->
            k "proof-file size: %d"
              (try Unix.((stat pfile).st_size) with _ -> 0));
        let res =
          let limits = Limit.All.mk ~time:(Limit.Time.mk ~h:1 ()) () in
          Run_prover_problem.run_proof_check ~limits ~proof_file:pfile prover
            checker pb
        in
        Run_event.mk_checker res)
      check_reqs
  in
  (* Clean up proof file if not kept and we ran all checks inline. *)
  (match proof_file with
  | Some file when not keep -> (try Sys.remove file with _ -> ())
  | _ -> ());
  Run_event.mk_prover result :: ev_proofs

let run_worker ?timeout ?memory (defs : Definitions.t) id socket_addr
    socket_port j =
  Log.debug (fun k -> k "(@[run_worker %d: started worker@])" id);
  let addr = Unix.ADDR_INET (socket_addr, socket_port) in
  let ic, oc = Unix.open_connection addr in
  Log.debug (fun k ->
      k "(@[run_worker %d: connected worker to server %a@])" id
        Misc.pp_unix_addr addr);
  let provers =
    List.fold_left
      (fun m (With_loc.{ view = Prover.{ name; _ }; _ } as p) ->
        Misc.Str_map.add name p m)
      Misc.Str_map.empty
      (Definitions.all_provers defs)
  in
  let checkers =
    List.fold_left
      (fun m (With_loc.{ view = Proof_checker.{ name; _ }; _ } as c) ->
        Misc.Str_map.add name c m)
      Misc.Str_map.empty
      (Definitions.all_checkers defs)
  in
  let time = Limit.Time.mk ~s:(CCOption.value ~default:0 timeout) () in
  let memory = Limit.Memory.mk ~m:(CCOption.value ~default:0 memory) () in
  let limits = Limit.All.mk ~time ~memory () in
  Log.debug (fun k -> k "(@[run_worker %d: started loop@])" id);
  try
    Log.debug (fun k -> k "(@[run_worker %d: sent initial message@])" id);
    Marshal.to_channel oc (Msg.Worker_response (id, [])) [];
    flush oc;
    while true do
      match Marshal.from_channel ic with
      | Msg.Worker_task cmds ->
        Log.debug (fun k ->
            k "(@[run_worker %d: received %d tasks@])" id (List.length cmds));
        let reps =
          List.flatten
          @@ Misc.Par_map.map_p ~j
               (fun (prover, pb) ->
                 run_prover_pb ?proof_dir:None ~limits ~prover ~pb provers
                   checkers)
               cmds
        in
        Log.debug (fun k ->
            k "(@[run_worker %d: sent %d responses@])" id (List.length reps));
        Marshal.to_channel oc (Msg.Worker_response (id, reps)) [];
        flush oc
      | Msg.Stop_worker -> raise Exit
    done
  with
  | Exit ->
    Log.debug (fun k ->
        k "(@[run_worker %d: received \"Stop\" message from master@])" id);
    close_out oc
  | End_of_file ->
    Log.debug (fun k ->
        k "(@[run_worker %d: master closed connection \"End_of_file\"@])" id);
    close_out oc
  | e ->
    Log.debug (fun k -> k "(@[run_worker %d: failure! sent exception@])" id);
    Marshal.to_channel oc (Msg.Worker_failure (id, e)) [];
    close_out oc;
    raise e

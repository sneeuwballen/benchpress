(** {1 Execute actions} *)

open Common
module Log = (val Logs.src_log (Logs.Src.create "benchpress.runexec-action"))

module Exec_run_provers : sig
  type t = Action.run_provers
  type jobs = Bounded of int | Cpus of int list

  type expanded = {
    j: jobs;
    problems: Problem.t list;
    provers: Prover.t list;
    checkers: Proof_checker.t Misc.Str_map.t;
    limits: Limit.All.t;
    proof_dir: string option; (* directory in which to store proofs *)
  }

  val expand :
    ?slurm:bool ->
    ?j:int ->
    ?cpus:int list ->
    ?dyn:bool ->
    ?limits:Limit.All.t ->
    ?proof_dir:string ->
    ?interrupted:(unit -> bool) ->
    Definitions.t ->
    Limit.All.t ->
    int option ->
    string option ->
    Subdir.t list ->
    Prover.t list ->
    expanded

  val run :
    ?timestamp:float ->
    ?on_start:(expanded -> unit) ->
    ?on_solve:(Test.result -> unit) ->
    ?on_start_proof_check:(unit -> unit) ->
    ?on_proof_check:(Test.proof_check_result -> unit) ->
    ?on_done:(Test_compact_result.t -> unit) ->
    ?interrupted:(unit -> bool) ->
    ?output:string ->
    ?update:bool ->
    uuid:Uuidm.t ->
    save:bool ->
    wal_mode:bool ->
    expanded ->
    Test_top_result.t lazy_t * Test_compact_result.t
  (** Run the given prover(s) on the given problem set, obtaining results
        after all the problems have been dealt with.
        @param on_solve called whenever a single problem is solved
        @param on_done called when the whole process is done
    *)

  val run_sbatch_job :
    ?timestamp:float ->
    ?on_start:(expanded -> unit) ->
    ?on_solve:(Test.result -> unit) ->
    ?on_start_proof_check:(unit -> unit) ->
    ?on_proof_check:(Test.proof_check_result -> unit) ->
    ?on_done:(Test_compact_result.t -> unit) ->
    ?interrupted:(unit -> bool) ->
    ?partition:string ->
    nodes:int ->
    addr:Unix.inet_addr ->
    port:int ->
    ntasks:int ->
    ?output:string ->
    ?update:bool ->
    uuid:Uuidm.t ->
    save:bool ->
    wal_mode:bool ->
    expanded ->
    Test_top_result.t lazy_t * Test_compact_result.t
end = struct
  type t = Action.run_provers
  type jobs = Bounded of int | Cpus of int list

  let ( >? ) a b =
    match a with
    | None -> b
    | Some x -> x

  let ( >?? ) a b =
    match a with
    | None -> b
    | Some _ as x -> x

  type expanded = {
    j: jobs;
    problems: Problem.t list;
    provers: Prover.t list;
    checkers: Proof_checker.t Misc.Str_map.t;
    limits: Limit.All.t;
    proof_dir: string option; (* directory in which to store proofs *)
  }

  let filter_regex_ = function
    | None -> fun _ -> true
    | Some re ->
      let re = Re.Perl.compile_pat re in
      fun path -> Re.execp re path

  (* turn a subdir into a list of problems *)
  let expand_subdir ?pattern ?(interrupted = fun _ -> false) ~dyn (s : Subdir.t)
      : Problem.t list =
    Error.guard (Error.wrapf "expand_subdir of_dir %a" Subdir.pp s) @@ fun () ->
    try
      let filter1 = filter_regex_ s.Subdir.inside.pattern in
      let filter2 = filter_regex_ pattern in
      let filter s = filter1 s && filter2 s in
      let files =
        CCIO.File.walk_l s.Subdir.path
        |> CCList.filter_map (fun (kind, f) ->
               if interrupted () then Error.fail "files.walk.interrupted";
               match kind with
               | `File when filter f -> Some f
               | _ -> None)
      in
      let n_files = List.length files in
      let n_done = ref 0 in
      files
      |> Misc.Par_map.map_p ~j:3 (fun path ->
             if interrupted () then failwith "interrupted";
             if dyn then
               Misc.synchronized (fun () ->
                   output_string stdout Misc.reset_line;
                   Printf.printf "[%6d/%6d] find expect for `%s`…%!" !n_done
                     n_files
                     (Misc.truncate_left 30 path));
             let res =
               Problem.make_find_expect path ~expect:s.Subdir.inside.expect
             in
             incr n_done;
             res)
    with
    | Error.E _ as e -> raise e
    | exn -> Error.(raise @@ of_exn exn)

  (* Expand options into concrete choices *)
  let expand ?(slurm = false) ?j ?cpus ?(dyn = false) ?limits ?proof_dir
      ?interrupted (defs : Definitions.t) s_limits s_j s_pattern s_dirs
      s_provers : expanded =
    let limits =
      match limits with
      | None -> s_limits
      | Some l -> Limit.All.with_defaults l ~defaults:s_limits
    in
    let j =
      match cpus with
      | None ->
        let j =
          j >?? s_j
          >?
          if slurm then
            0
          else
            Misc.guess_cpu_count ()
        in
        Bounded j
      | Some cpus -> Cpus cpus
    in
    let problems =
      CCList.flat_map
        (expand_subdir ?pattern:s_pattern ~dyn ?interrupted)
        s_dirs
    in
    let checkers =
      Definitions.all_checkers defs
      |> List.map (fun c ->
             let c = c.With_loc.view in
             c.Proof_checker.name, c)
      |> Misc.Str_map.of_list
    in
    { j; limits; problems; checkers; proof_dir; provers = s_provers }

  let _nop _ = ()

  let copy_problem ~proof_dir ~prover (file : string) : unit =
    let basename = spf "pb-%s-%s" prover.Prover.name (Filename.basename file) in
    let new_path = Filename.concat proof_dir basename in
    Log.debug (fun k -> k "(@[copy-problem@ :from %S@ :to %S@])" file new_path);
    CCIO.with_out new_path @@ fun oc ->
    CCIO.with_in file @@ fun ic -> CCIO.copy_into ~bufsize:(64 * 1024) ic oc

  (* run [f], ensuring [proof_file] is cleaned up afterwards if it exists *)
  let with_proof_file_opt ~proof_file ~keep f =
    match proof_file with
    | Some file when not keep ->
      CCFun.finally ~h:(fun () -> try Sys.remove file with _ -> ()) ~f
    | _ -> f ()

  let prepare_db ~wal_mode ?output ~update timestamp uuid save provers =
    let db =
      if save then (
        let db_file =
          match output with
          | Some output ->
            if Sys.file_exists output then
              if update then (
                Sys.remove output;
                output
              ) else
                Error.failf "The file %s exists" output
            else
              output
          | None -> Misc.file_for_uuid "res" ~timestamp uuid "sqlite"
        in
        Log.debug (fun k -> k "output database file %s" db_file);
        let db = Sqlite3.db_open ~mutex:`FULL db_file in
        if wal_mode then (
          match Db.exec0 db "pragma journal_mode=WAL;" with
          | Ok _ -> db
          | Error rc -> Error.raise (Misc.err_of_db rc)
        ) else
          db
      ) else
        Sqlite3.db_open ":memory:"
    in
    let ms =
      match Sys.getenv "BENCHPRESS_BUSY_TIMEOUT" with
      | n ->
        (try int_of_string n
         with _e -> Error.fail "BENCHPRESS_BUSY_TIMEOUT must be an integer")
      | exception Not_found -> 3000
    in
    Db.setup_timeout db ~ms;
    Test_top_result.db_prepare db;
    Test_metadata.to_db db
      {
        Test_metadata.timestamp = Some timestamp;
        uuid;
        total_wall_time = None;
        n_bad = 0;
        n_results = 0;
        dirs = [];
        provers = [];
      };
    ( Error.guard (Error.wrap "inserting provers into DB") @@ fun () ->
      List.iter (Prover.to_db db) provers );
    db

  let run ?(timestamp = Misc.now_s ()) ?(on_start = _nop) ?(on_solve = _nop)
      ?(on_start_proof_check = _nop) ?(on_proof_check = _nop) ?(on_done = _nop)
      ?(interrupted = fun _ -> false) ?output ?(update = false) ~uuid ~save
      ~wal_mode (self : expanded) : _ * _ =
    let start = Misc.now_s () in
    (* prepare DB *)
    let db =
      prepare_db ~wal_mode ?output ~update timestamp uuid save self.provers
    in
    on_start self;

    CCOpt.iter Misc.mkdir_rec self.proof_dir;

    let run_prover_pb ~prover ~pb ~db () : _ list =
      (* Also runs the proof checker, if the prover is proof producing
         and returns "UNSAT". *)
      if interrupted () then Error.fail "run.interrupted";
      Error.guard
        (Error.wrapf "(@[running :prover %a :on %a@])" Prover.pp_name prover
           Problem.pp pb)
      @@ fun () ->
      let proof_file, keep =
        if prover.Prover.produces_proof then (
          let pb = pb.Problem.name in
          let ext = CCOpt.get_or ~default:"proof" prover.Prover.proof_ext in
          let basename =
            spf "proof-%s-%s.%s" prover.Prover.name (Filename.basename pb) ext
          in
          let filename, keep =
            match self.proof_dir with
            | None -> Filename.concat (Filename.dirname pb) basename, false
            | Some dir ->
              (* copy problem *)
              copy_problem ~proof_dir:dir ~prover pb;
              (* user asked to keep proofs, in given directory. *)
              Filename.concat dir basename, true
          in
          Some filename, keep
        ) else
          None, false
      in

      (* continue but ensure we cleanup the proof file *)
      with_proof_file_opt ~proof_file ~keep @@ fun () ->
      let result =
        Run_prover_problem.run ~limits:self.limits ~proof_file prover pb
      in
      (* insert into DB here *)
      let ev_prover = Run_event.mk_prover result in
      CCLock.with_lock db (fun db -> Run_event.to_db db ev_prover);

      let ev_proof =
        match result.res, proof_file with
        | Res.Unsat, Some pfile when prover.Prover.produces_proof ->
          (* run proof checker *)
          Log.debug (fun k ->
              k "proof-file size: %d"
                (try Unix.((stat pfile).st_size) with _ -> 0));

          on_start_proof_check ();

          let checker =
            match prover.Prover.proof_checker with
            | None -> Error.failf "cannot check proofs for '%s'" prover.name
            | Some c ->
              (try Misc.Str_map.find c self.checkers
               with Not_found ->
                 Error.failf "cannot find proof checker '%s'" c)
          in

          let res =
            let limits = Limit.All.mk ~time:(Limit.Time.mk ~h:1 ()) () in
            Run_prover_problem.run_proof_check ~limits ~proof_file:pfile prover
              checker pb
          in
          let ev_checker = Run_event.mk_checker res in
          on_proof_check res;

          (* insert into DB here *)
          CCLock.with_lock db (fun db -> Run_event.to_db db ev_checker);
          [ ev_checker ]
        | _ -> []
      in

      on_solve result;

      (* only now we can announce our "solve" result *)
      Run_event.mk_prover result :: ev_proof
    in

    (* build list of tasks *)
    let jobs =
      CCList.flat_map
        (fun pb -> CCList.map (fun prover -> prover, pb) self.provers)
        self.problems
    in
    (* run provers *)
    let res_l =
      let db = CCLock.create db in
      match self.j with
      | Bounded j ->
        Misc.Par_map.map_p ~j
          (fun (prover, pb) -> run_prover_pb ~prover ~pb ~db ())
          jobs
        |> CCList.flatten
      | Cpus cpus ->
        Misc.Par_map.map_with_resource ~resources:cpus
          (fun cpu (prover, pb) ->
            Log.debug (fun m -> m "Running on cpu %d" cpu);
            Misc.with_affinity cpu (run_prover_pb ~prover ~pb ~db))
          jobs
        |> CCList.flatten
    in

    if interrupted () then Error.fail "run.interrupted";
    let total_wall_time = Misc.now_s () -. start in
    let uuid = uuid in
    Logs.info (fun k ->
        k "benchmark done in %a, uuid=%a" Misc.pp_human_duration total_wall_time
          Uuidm.pp uuid);
    let timestamp = Some timestamp in
    let total_wall_time = Some total_wall_time in
    let meta =
      {
        Test_metadata.uuid;
        timestamp;
        total_wall_time;
        n_results = 0;
        dirs = [];
        n_bad = 0;
        provers = List.map Prover.name self.provers;
      }
    in
    Logs.debug (fun k -> k "saving metadata…");
    Test_metadata.to_db db meta;
    let top_res =
      lazy
        (let provers = CCList.map fst jobs in
         Test_top_result.make ~analyze_full:true ~meta ~provers res_l)
    in
    let r = Test_compact_result.of_db ~full:true db in
    on_done r;
    Logs.debug (fun k -> k "closing db…");
    ignore (Sqlite3.db_close db : bool);
    top_res, r

  let run_sbatch_job ?(timestamp = Misc.now_s ()) ?(on_start = _nop)
      ?(on_solve = _nop) ?(on_start_proof_check = _nop) ?(on_proof_check = _nop)
      ?(on_done = _nop) ?(interrupted = fun _ -> false) ?partition ~nodes ~addr
      ~port ~ntasks ?output ?(update = false) ~uuid ~save ~wal_mode
      (self : expanded) : _ * _ =
    ignore on_start_proof_check;
    let j =
      match self.j with
      | Bounded j -> j
      | Cpus cpus ->
        Log.warn (fun m -> m "cpu affinity ignored in slurm mode");
        List.length cpus
    in
    let start = Misc.now_s () in
    let db =
      prepare_db ~wal_mode ?output ~update timestamp uuid save self.provers
    in
    on_start self;
    let jobs =
      CCList.flat_map
        (fun pb ->
          CCList.map (fun prover -> prover.Prover.name, pb) self.provers)
        self.problems
    in
    let config_file = Misc.file_for_uuid "config" ~timestamp uuid "sexp" in
    let sexps =
      Misc.Str_map.fold
        (fun _ c acc ->
          Stanza.proof_checker_wl_to_st (With_loc.make ~loc:Loc.none c) :: acc)
        self.checkers
        (List.fold_left
           (fun acc p ->
             Stanza.prover_wl_to_st (With_loc.make ~loc:Loc.none p) :: acc)
           [] self.provers)
    in
    CCIO.with_out config_file (fun oc ->
        let ocf = Format.formatter_of_out_channel oc in
        List.iter (Format.fprintf ocf "%a@." Stanza.pp) (List.rev sexps));

    let get_tasks =
      let jobs_ref = ref jobs in
      let jobs_lock = Mutex.create () in
      fun j ->
        Mutex.lock jobs_lock;
        let rec aux j acc jobs =
          match jobs with
          | _ when j <= 0 -> acc, jobs
          | h :: t -> aux (j - 1) (h :: acc) t
          | [] -> acc, []
        in
        let tasks, jobs = aux j [] !jobs_ref in
        jobs_ref := jobs;
        Mutex.unlock jobs_lock;
        tasks
    in
    let add_resps, get_resps, get_nb_resps =
      let nb_resps = ref 0 in
      let resps_ref = ref [] in
      let resps_lock = Mutex.create () in
      let db_wl = CCLock.create db in
      let add_resps evl =
        Mutex.lock resps_lock;
        nb_resps := !nb_resps + List.length evl;
        resps_ref := List.rev_append evl !resps_ref;
        List.iter
          (fun ev ->
            (match ev with
            | Run_event.Prover_run r -> on_solve r
            | Checker_run r -> on_proof_check r);
            CCLock.with_lock db_wl (fun db -> Run_event.to_db db ev))
          evl;
        Mutex.unlock resps_lock
      in
      let get_resps () =
        Mutex.lock resps_lock;
        let res = !resps_ref in
        Mutex.unlock resps_lock;
        res
      in
      let get_nb_resps () =
        Mutex.lock resps_lock;
        let res = List.length !resps_ref in
        Mutex.unlock resps_lock;
        res
      in
      add_resps, get_resps, get_nb_resps
    in
    let wdms_set_true, get_wdms =
      let work_done_msg_sent = ref false in
      let work_done_msg_sent_mutex = Mutex.create () in
      let wdms_set_true () =
        Mutex.lock work_done_msg_sent_mutex;
        work_done_msg_sent := true;
        Mutex.unlock work_done_msg_sent_mutex
      in
      let get_wdms () =
        Mutex.lock work_done_msg_sent_mutex;
        let res = !work_done_msg_sent in
        Mutex.unlock work_done_msg_sent_mutex;
        res
      in
      wdms_set_true, get_wdms
    in
    let incr_nb_workers, decr_nb_workers, get_nb_workers =
      let nb_workers = ref 0 in
      let nb_workers_mutex = Mutex.create () in
      let aux f () =
        Mutex.lock nb_workers_mutex;
        f nb_workers;
        Mutex.unlock nb_workers_mutex
      in
      ( aux incr,
        aux decr,
        fun () ->
          Mutex.lock nb_workers_mutex;
          let res = !nb_workers in
          Mutex.unlock nb_workers_mutex;
          res )
    in
    let nb_jobs = List.length jobs in

    let resp_sock_path =
      Misc.file_for_uuid "benchpress" ~dir:Xdg.runtime_dir ~timestamp uuid
        "sock"
    in
    let resp_sock_addr = Unix.ADDR_UNIX resp_sock_path in

    let server_loop ic oc =
      Log.debug (fun k -> k "(@[server_loop@ : started.@])");
      incr_nb_workers ();
      try
        while true do
          match Marshal.from_channel ic with
          | Msg.Worker_response (id, evl) ->
            Log.debug (fun k ->
                k "(@[server_loop@ : Worker %d sent %d responses.@])" id
                  (List.length evl));
            if evl <> [] then add_resps evl;
            let tasks = get_tasks ntasks in
            if tasks = [] then (
              Marshal.to_channel oc Msg.Stop_worker [];
              flush oc;
              Log.debug (fun k ->
                  k
                    "(@[server_loop@ : Sent \"Stop_worker\" to Worker %d. No \
                     more tasks left.@])"
                    id)
            ) else (
              Marshal.to_channel oc (Msg.Worker_task tasks) [];
              flush oc;
              Log.debug (fun k ->
                  k "(@[server_loop@ : Sent %d tasks to Worker %d@])"
                    (List.length tasks) id)
            )
          | Msg.Worker_failure (id, e) ->
            Log.err (fun k -> k "(@[server_loop@ : Worker %d failed!@])" id);
            raise e
        done
      with
      | End_of_file ->
        Log.debug (fun k ->
            k "(@[server_loop@ : Worker closed the connection.@])");
        close_out oc;
        decr_nb_workers ();
        if
          get_nb_workers () = 0
          && get_nb_resps () = nb_jobs
          && not (get_wdms ())
        then (
          wdms_set_true ();
          let _, oc =
            Unix.handle_unix_error Unix.open_connection resp_sock_addr
          in
          Marshal.to_channel oc Msg.Work_done [];
          flush oc;
          Log.debug (fun k ->
              k "(@[server_loop@ : Sent \"Work_done\" to parent proc.@])");
          close_out oc
        )
      | e ->
        Log.err (fun k -> k "(@[server_loop@ : Worker failed!@])");
        close_out oc;
        decr_nb_workers ();
        raise e
    in

    let sock, used_port = Misc.mk_socket (Unix.ADDR_INET (addr, port)) in
    ignore (CCThread.spawn (fun () -> Misc.start_server nodes server_loop sock));

    Log.debug (fun k ->
        k "Spawned the thread that establishes a server listening at: %s:%s."
          (Unix.string_of_inet_addr addr)
          used_port);
    let sbatch_cmds =
      Slurm_cmd.mk_sbatch_cmds self.limits self.proof_dir j addr used_port
        partition config_file nodes
    in
    let job_ids =
      List.fold_left
        (fun acc cmd ->
          let rpres = Run_proc.run cmd in
          let job_id = int_of_string (String.trim rpres.stdout) in
          Log.debug (fun k -> k "Submitted the job %d to slurm@." job_id);
          job_id :: acc)
        [] sbatch_cmds
    in
    Log.debug (fun k ->
        k "Submitted %d launch worker scripts to slurm." (List.length job_ids));
    Log.debug (fun k -> k "Waiting for the \"Work_done\" message.");
    Misc.establish_server 1
      (fun ic _ ->
        match Marshal.from_channel ic with
        | Msg.Work_done -> ())
      resp_sock_addr;
    Log.debug (fun k -> k "Received the \"Work_done\" message.");

    List.iter
      (fun job_id -> ignore (Sys.command (Slurm_cmd.scancel job_id)))
      job_ids;
    Log.debug (fun k ->
        k "Killed the workers that are still alive or didn't start yet.");

    Sys.remove config_file;
    Sys.remove resp_sock_path;
    Log.debug (fun k ->
        k "Deleted the generated config_file and linux socket file.");

    if interrupted () then Error.fail "run.interrupted";
    let total_wall_time = Misc.now_s () -. start in
    let uuid = uuid in
    Logs.info (fun k ->
        k "benchmark done in %a, uuid=%a" Misc.pp_human_duration total_wall_time
          Uuidm.pp uuid);
    let timestamp = Some timestamp in
    let total_wall_time = Some total_wall_time in
    let meta =
      {
        Test_metadata.uuid;
        timestamp;
        total_wall_time;
        n_results = 0;
        dirs = [];
        n_bad = 0;
        provers = List.map Prover.name self.provers;
      }
    in
    Logs.debug (fun k -> k "saving metadata…");
    Test_metadata.to_db db meta;
    let top_res =
      lazy
        (let provers_map =
           List.fold_left
             (fun m ({ Prover.name; _ } as p) -> Misc.Str_map.add name p m)
             Misc.Str_map.empty self.provers
         in
         let provers =
           List.map (fun (pn, _) -> Misc.Str_map.find pn provers_map) jobs
         in
         Test_top_result.make ~analyze_full:true ~meta ~provers (get_resps ()))
    in
    let r = Test_compact_result.of_db db in
    on_done r;
    Logs.debug (fun k -> k "closing db…");
    ignore (Sqlite3.db_close db : bool);
    top_res, r
end

type cb_progress =
  < on_progress : percent:int -> elapsed_time:float -> eta:float -> unit
  ; on_done : unit >

module Progress_run_provers : sig
  type t =
    < on_res : Run_prover_problem.job_res -> unit
    ; on_start_proof_check : unit
    ; on_proof_check_res : Test.proof_check_result -> unit
    ; on_done : unit >

  val nil : t

  val make :
    ?cb_progress:cb_progress ->
    ?pp_results:bool ->
    ?dyn:bool ->
    Exec_run_provers.expanded ->
    t
  (** Make a progress tracker.
      @param dyn if true, print a progress bar in the terminal
      @param pp_results if true, print each individual result as it's found
      @param on_progress callback when progress is made, with a percentage and ETA
  *)
end = struct
  type t =
    < on_res : Run_prover_problem.job_res -> unit
    ; on_start_proof_check : unit
    ; on_proof_check_res : Test.proof_check_result -> unit
    ; on_done : unit >

  let nil : t =
    object
      method on_res _ = ()
      method on_start_proof_check = ()
      method on_proof_check_res _ = ()
      method on_done = ()
    end

  (* callback that prints a result *)
  let progress_dynamic len =
    let start = Misc.now_s () in
    let len = ref len in
    let count = ref 0 in
    let tick () = incr count in
    let get_state () =
      let time_elapsed = Misc.now_s () -. start in
      let percent =
        if !len = 0 then
          100.
        else
          float_of_int !count *. 100. /. float_of_int !len
      in
      (* elapsed=(percent/100)*total, so total=elapsed*100/percent; eta=total-elapsed *)
      let eta = time_elapsed *. (100. -. percent) /. percent in
      percent, time_elapsed, eta
    in
    let bump () = incr len in
    let pp_bar () =
      let len_bar = 50 in
      let bar =
        String.init len_bar (fun i ->
            if i * !len <= len_bar * !count then
              '#'
            else
              '-')
      in
      let percent, time_elapsed, eta = get_state () in
      Misc.synchronized (fun () ->
          Format.printf "... %5d/%d | %3.1f%% [%6s: %s] [eta %6s]@?" !count !len
            percent
            (Misc.human_duration time_elapsed)
            bar (Misc.human_duration eta));
      if !count = !len then Misc.synchronized (fun () -> Format.printf "@.")
    in
    pp_bar, get_state, bump, tick

  let progress ~w_prover ~w_pb ?cb_progress ~pp_results ~dyn n : t =
    let pp_bar, get_state, bump, tick = progress_dynamic n in
    let pp_common_ () =
      if dyn then (
        output_string stdout Misc.reset_line;
        pp_bar ()
      );
      CCOpt.iter
        (fun cb ->
          let percent, elapsed_time, eta = get_state () in
          cb#on_progress ~percent:(int_of_float percent) ~elapsed_time ~eta)
        cb_progress;
      ()
    in
    object
      method on_res res =
        tick ();
        if pp_results then
          Run_prover_problem.pp_result_progress ~w_prover ~w_pb res;
        pp_common_ ()

      method on_start_proof_check =
        bump ();
        (* add another task *)
        pp_common_ ()

      method on_proof_check_res res =
        if pp_results then
          Run_prover_problem.pp_check_result_progress ~w_prover ~w_pb res;
        pp_common_ ()

      method on_done =
        pp_common_ ();
        match cb_progress with
        | None -> ()
        | Some cb -> cb#on_done
    end

  let make ?cb_progress ?(pp_results = true) ?(dyn = false)
      (r : Exec_run_provers.expanded) : t =
    match cb_progress, pp_results, dyn with
    | None, false, false -> nil
    | _ ->
      let len = List.length r.problems in
      let w_prover =
        List.fold_left
          (fun m p -> max m (String.length (Prover.name p) + 1))
          0 r.provers
        |> min 25
      and w_pb =
        List.fold_left
          (fun m pb -> max m (String.length pb.Problem.name + 1))
          0 r.problems
        |> min 60
      in
      progress ~w_prover ~w_pb ?cb_progress ~pp_results ~dyn
        (len * List.length r.provers)
end

let dump_results_sqlite (results : Test_top_result.t) : unit =
  let uuid = results.Test_top_result.meta.uuid in
  (* save results *)
  let dump_file =
    let filename =
      Printf.sprintf "res-%s-%s.sqlite"
        (CCOpt.map_or ~default:"date" Misc.human_datetime
           results.Test_top_result.meta.timestamp)
        (Uuidm.to_string uuid)
    in
    let data_dir = Filename.concat (Xdg.data_dir ()) !Xdg.name_of_project in
    (try Unix.mkdir data_dir 0o744 with _ -> ());
    Filename.concat data_dir filename
  in
  Logs.app (fun k -> k "write results into sqlite DB `%s`" dump_file);
  (try
     Db.with_db ~timeout:500 dump_file (fun db ->
         Test_top_result.to_db db results)
   with
  | Error.E e ->
    Logs.err (fun k -> k "error when saving to %s:@ %a" dump_file Error.pp e);
    exit 1
  | e ->
    Logs.err (fun k ->
        k "error when saving to %s:@ %s" dump_file (Printexc.to_string e));
    exit 1);
  ()

let with_chdir d f =
  let cur_dir = Sys.getcwd () in
  Sys.chdir d;
  try
    let x = f () in
    Sys.chdir cur_dir;
    x
  with e ->
    Sys.chdir cur_dir;
    raise e

let run_cmd ?loc s : unit =
  let c = Sys.command s in
  if c = 0 then
    ()
  else
    Error.failf ?loc "command %S returned with error code %d" s c

module Git_checkout = struct
  type t = Action.git_checkout

  let run (self : t) : unit =
    Error.guard (Error.wrapf "running action git-checkout '%s'" self.ref)
    @@ fun () ->
    let { Action.dir; ref; fetch_first; loc = _ } = self in
    with_chdir dir (fun () ->
        (match fetch_first with
        | Some Git_fetch -> run_cmd "git fetch"
        | Some Git_pull -> run_cmd "git pull --ff-only"
        | _ -> ());
        run_cmd ("git checkout " ^ ref))
end

(** Run the given action *)
let rec run ?output ?(save = true) ?interrupted ?cb_progress
    (defs : Definitions.t) (a : Action.t) : unit =
  Error.guard (Error.wrapf "running action %a" Action.pp a) @@ fun () ->
  match a with
  | Action.Act_run_provers r ->
    let is_dyn =
      CCOpt.get_or ~default:false @@ Definitions.option_progress defs
    in
    let r_expanded =
      Exec_run_provers.expand ?interrupted ~dyn:is_dyn
        ?j:(Definitions.option_j defs)
        defs r.limits r.j r.pattern r.dirs r.provers
    in
    let progress =
      Progress_run_provers.make ~pp_results:true ~dyn:is_dyn ?cb_progress
        r_expanded
    in
    let uuid = Misc.mk_uuid () in
    let res =
      Exec_run_provers.run ?interrupted ~on_solve:progress#on_res
        ~on_proof_check:progress#on_proof_check_res
        ~on_done:(fun _ -> progress#on_done)
        ?output ~save ~wal_mode:false ~timestamp:(Misc.now_s ()) ~uuid
        r_expanded
    in
    Format.printf "task done: %a@." Test_compact_result.pp res;
    ()
  | Act_run_slurm_submission
      {
        nodes;
        j;
        dirs;
        provers;
        pattern;
        limits;
        partition;
        addr;
        port;
        ntasks;
        _;
      } ->
    let is_dyn =
      CCOpt.get_or ~default:false @@ Definitions.option_progress defs
    in
    let r_expanded =
      Exec_run_provers.expand ~slurm:true ?interrupted ~dyn:is_dyn
        ?j:(Definitions.option_j defs)
        defs limits j pattern dirs provers
    in
    let progress =
      Progress_run_provers.make ~pp_results:true ~dyn:is_dyn ?cb_progress
        r_expanded
    in
    let uuid = Misc.mk_uuid () in
    let res =
      Exec_run_provers.run_sbatch_job ~timestamp:(Misc.now_s ()) ?interrupted
        ~on_solve:progress#on_res ~on_proof_check:progress#on_proof_check_res
        ~on_done:(fun _ -> progress#on_done)
        ?output ~save ~uuid ~wal_mode:false ?partition ~ntasks ~nodes ~addr
        ~port r_expanded
    in
    Format.printf "task done: %a@." Test_compact_result.pp res;
    ()
  | Action.Act_progn l ->
    List.iter (fun a -> run ?output ~save ?interrupted defs a) l
  | Action.Act_git_checkout git -> Git_checkout.run git
  | Action.Act_run_cmd { cmd; loc } -> run_cmd ~loc cmd

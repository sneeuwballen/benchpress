
(** {1 Execute actions} *)

open Common
module Log = (val Logs.src_log (Logs.Src.create "benchpress.runexec-action"))

(** File for results with given uuid and timestamp *)
let db_file_for_uuid ~timestamp (uuid:Uuidm.t) : string =
  let filename =
    Printf.sprintf "res-%s-%s.sqlite"
      (match Ptime.of_float_s timestamp with
       | None -> Printf.sprintf "<time %.1fs>" timestamp
       | Some t -> Misc.datetime_compact t)
      (Uuidm.to_string uuid)
  in
  let data_dir = Filename.concat (Xdg.data_dir ()) !(Xdg.name_of_project) in
  (try Unix.mkdir data_dir 0o744 with _ -> ());
  Filename.concat data_dir filename

module Exec_run_provers : sig
  type t = Action.run_provers

  type expanded = {
    j: int;
    problems: Problem.t list;
    provers: Prover.t list;
    checkers: Proof_checker.t Misc.Str_map.t;
    limits : Limit.All.t;
  }

  val expand :
    ?j:int ->
    ?dyn:bool ->
    ?limits:Limit.All.t ->
    ?interrupted:(unit -> bool) ->
    Definitions.t ->
    t -> expanded

  val run :
    ?timestamp:float ->
    ?on_start:(expanded -> unit) ->
    ?on_solve:(Test.result -> unit) ->
    ?on_proof_check:(Test.proof_check_result -> unit) ->
    ?on_done:(Test_compact_result.t -> unit) ->
    ?interrupted:(unit -> bool) ->
    uuid:Uuidm.t ->
    save:bool ->
    expanded ->
    Test_top_result.t lazy_t * Test_compact_result.t
    (** Run the given prover(s) on the given problem set, obtaining results
        after all the problems have been dealt with.
        @param on_solve called whenever a single problem is solved
        @param on_done called when the whole process is done
    *)
end = struct
  type t = Action.run_provers
  let (>?) a b = match a with None -> b | Some x -> x
  let (>??) a b = match a with None -> b | Some _ as x -> x

  type expanded = {
    j: int;
    problems: Problem.t list;
    provers: Prover.t list;
    checkers: Proof_checker.t Misc.Str_map.t;
    limits : Limit.All.t;
  }

  let filter_regex_ = function
    | None -> (fun _ -> true)
    | Some re ->
      let re = Re.Perl.compile_pat re in
      (fun path -> Re.execp re path)

  (* turn a subdir into a list of problems *)
  let expand_subdir ?pattern ?(interrupted=fun _->false)
      ~dyn (s:Subdir.t) : Problem.t list =
    Error.guard (Error.wrapf "expand_subdir of_dir %a" Subdir.pp s) @@ fun() ->
    try
      let filter1 = filter_regex_ s.Subdir.inside.pattern in
      let filter2 = filter_regex_ pattern in
      let filter s = filter1 s && filter2 s in
      let files =
        CCIO.File.walk_l s.Subdir.path
        |> CCList.filter_map
          (fun (kind,f) ->
             if interrupted() then Error.fail "files.walk.interrupted";
             match kind with
             | `File when filter f -> Some f
             | _ -> None)
      in
      let n_files = List.length files in
      let n_done = ref 0 in
      files
      |> Misc.Par_map.map_p ~j:3
        (fun path ->
           if interrupted() then failwith "interrupted";
           if dyn then (
             Misc.synchronized (fun () ->
                 output_string stdout Misc.reset_line;
                 Printf.printf "[%6d/%6d] find expect for `%s`…%!"
                   !n_done n_files (Misc.truncate_left 30 path);
               )
           );
           let res =Problem.make_find_expect path ~expect:s.Subdir.inside.expect in
           incr n_done;
           res)
        with
        | Error.E _ as e -> raise e
        | exn -> Error.(raise @@ of_exn exn)

  (* Expand options into concrete choices *)
  let expand ?j ?(dyn=false) ?limits ?interrupted (defs:Definitions.t) (self:t) : expanded =
    let limits = match limits with
      | None -> self.limits
      | Some l -> Limit.All.with_defaults l ~defaults:self.limits
    in
    let j = j >?? self.j >? Misc.guess_cpu_count () in
    let problems = CCList.flat_map (expand_subdir ~dyn ?interrupted) self.dirs in
    let checkers =
      Definitions.all_checkers defs
      |> List.map (fun c -> let c=c.With_loc.view in c.Proof_checker.name, c)
      |> Misc.Str_map.of_list
    in
    { j; limits; problems; checkers; provers=self.provers; }

  let _nop _ = ()

  let with_proof_file_opt ~proof_file f =
    match proof_file with
    | None -> f ()
    | Some file ->
      CCFun.finally
        ~h:(fun () -> try Sys.remove file with _ -> ())
        ~f

  let run ?(timestamp=Misc.now_s())
      ?(on_start=_nop) ?(on_solve = _nop) ?(on_proof_check = _nop) ?(on_done = _nop)
      ?(interrupted=fun _->false)
      ~uuid ~save
      (self:expanded) : _*_ =
    let start = Misc.now_s() in
    (* prepare DB *)
    let db =
      if save then (
        let db_file = db_file_for_uuid ~timestamp uuid in
        Sqlite3.db_open ~mutex:`FULL db_file
      ) else
        Sqlite3.db_open ":memory:"
    in
    let ms =
      match Sys.getenv "BENCHPRESS_BUSY_TIMEOUT" with
      | n ->
        (try int_of_string n
         with _e ->
          Error.fail "BENCHPRESS_BUSY_TIMEOUT must be an integer")
      | exception Not_found -> 3000
    in
    Db.setup_timeout db ~ms;
    Test_top_result.db_prepare db;
    Test_metadata.to_db db
      {Test_metadata.timestamp=Some timestamp; uuid; total_wall_time=None; n_bad=0;
       n_results=0; dirs=[]; provers=[]};
    begin
      Error.guard (Error.wrap "inserting provers into DB") @@ fun () ->
      List.iter (Prover.to_db db) self.provers;
    end;
    on_start self;
    (* build list of tasks *)
    let jobs =
      CCList.flat_map
        (fun pb -> CCList.map (fun prover -> prover,pb) self.provers)
        self.problems
    in
    (* run provers *)
    let res_l =
      let db = CCLock.create db in
      Misc.Par_map.map_p ~j:self.j
        (fun (prover,pb) ->
           (* Also runs the proof checker, if the prover is proof producing
              and returns "UNSAT". *)
           if interrupted() then Error.fail "run.interrupted";
           Error.guard
             (Error.wrapf "(@[running :prover %a :on %a@])"
                Prover.pp_name prover Problem.pp pb) @@ fun () ->

           let proof_file =
             if prover.Prover.produces_proof then (
               let pb = pb.Problem.name in
               let ext = CCOpt.get_or ~default:"proof" prover.Prover.proof_ext in
               let f =
                 Filename.concat (Filename.dirname pb)
                   (spf "proof-%s-%s.%s" prover.Prover.name (Filename.basename pb) ext)
               in
               Some f
             ) else None
           in

           (* continue but ensure we cleanup the proof file *)
           with_proof_file_opt ~proof_file @@ fun () ->

           let result =
             Run_prover_problem.run ~limits:self.limits ~proof_file
               prover pb
           in
           (* insert into DB here *)
           let ev_prover = Run_event.mk_prover result in
           CCLock.with_lock db (fun db -> Run_event.to_db db ev_prover);
           on_solve result; (* callback *)

           let ev_proof =
             match result.res, proof_file with
             | Res.Unsat, Some pfile when prover.Prover.produces_proof ->
               (* run proof checker *)
               Log.debug (fun k->k"proof-file size: %d"
                             (try Unix.((stat pfile).st_size) with _ -> 0));

               let checker =
                 match prover.Prover.proof_checker with
                 | None -> Error.failf "cannot check proofs for '%s'" prover.name
                 | Some c ->
                   try Misc.Str_map.find c self.checkers
                   with Not_found ->
                     Error.failf "cannot find proof checker '%s'" c
               in

               let res =
                 let limits = Limit.All.mk ~time:(Limit.Time.mk ~h:1 ()) () in
                 Run_prover_problem.run_proof_check ~limits
                   ~proof_file:pfile prover checker pb
               in
               let ev_checker = Run_event.mk_checker res in
               on_proof_check res;

               (* insert into DB here *)
               CCLock.with_lock db (fun db -> Run_event.to_db db ev_checker);
               [ev_checker]

             | _ -> []
           in

           Run_event.mk_prover result :: ev_proof
        )
        jobs

      |> CCList.flatten
    in

    if interrupted() then (
      Error.fail "run.interrupted";
    );
    let total_wall_time = Misc.now_s() -. start in
    let uuid = uuid in
    Logs.info (fun k->k"benchmark done in %a, uuid=%a"
                  Misc.pp_human_duration total_wall_time
                  Uuidm.pp uuid);
    let timestamp = Some timestamp in
    let total_wall_time = Some total_wall_time in
    let meta = {
      Test_metadata.uuid; timestamp; total_wall_time; n_results=0; dirs=[]; n_bad=0;
      provers=List.map Prover.name self.provers;
    } in
    Logs.debug (fun k->k "saving metadata…");
    Test_metadata.to_db db meta;
    let top_res = lazy (
      let provers = CCList.map fst jobs in
      Test_top_result.make ~meta ~provers res_l
    ) in
    let r = Test_compact_result.of_db db in
    on_done r;
    Logs.debug (fun k->k "closing db…");
    ignore (Sqlite3.db_close db : bool);
    top_res, r
end

type cb_progress = <
  on_progress: percent:int -> elapsed_time:float -> eta:float -> unit;
  on_done: unit;
>

module Progress_run_provers : sig
  type t = <
    on_res: Run_prover_problem.job_res -> unit;
    on_proof_check_res: Test.proof_check_result -> unit;
    on_done: unit;
  >
  val nil : t
  val make :
    ?cb_progress:cb_progress ->
    ?pp_results:bool ->
    ?dyn:bool ->
    Exec_run_provers.expanded -> t
  (** Make a progress tracker.
      @param dyn if true, print a progress bar in the terminal
      @param pp_results if true, print each individual result as it's found
      @param on_progress callback when progress is made, with a percentage and ETA
  *)
end = struct
  type t = <
    on_res: Run_prover_problem.job_res -> unit;
    on_proof_check_res: Test.proof_check_result -> unit;
    on_done: unit;
  >

  let nil = object
    method on_res _=()
    method on_proof_check_res _=()
    method on_done=()
  end

  (* callback that prints a result *)
  let progress_dynamic len =
    let start = Misc.now_s() in
    let count = ref 0 in
    let tick() = incr count in
    let get_state() =
      let time_elapsed = Misc.now_s() -. start in
      let percent = if len=0 then 100. else (float_of_int !count *. 100.) /. float_of_int len in
      (* elapsed=(percent/100)*total, so total=elapsed*100/percent; eta=total-elapsed *)
      let eta = time_elapsed *. (100. -. percent) /. percent in
      percent, time_elapsed, eta
    in
    let pp_bar () =
      let len_bar = 50 in
      let bar = String.init len_bar
          (fun i -> if i * len <= len_bar * !count then '#' else '-') in
      let percent, time_elapsed, eta = get_state() in
      Misc.synchronized
        (fun () ->
           Format.printf "... %5d/%d | %3.1f%% [%6s: %s] [eta %6s]@?"
             !count len percent (Misc.human_duration time_elapsed) bar (Misc.human_duration eta));
      if !count = len then (
        Misc.synchronized (fun() -> Format.printf "@.")
      )
    in
    pp_bar, get_state, tick

  let progress ~w_prover ~w_pb ?cb_progress ~pp_results ~dyn n : t =
    let pp_bar, get_state, tick = progress_dynamic n in
    let pp_common_ () =
      if dyn then (
        output_string stdout Misc.reset_line;
        pp_bar ();
      );
      CCOpt.iter
        (fun cb ->
           let percent, elapsed_time, eta = get_state() in
           cb#on_progress ~percent:(int_of_float percent) ~elapsed_time ~eta)
        cb_progress;
      ()
    in
    object
      method on_res res =
        tick();
        if pp_results then Run_prover_problem.pp_result_progress ~w_prover ~w_pb res;
        pp_common_();
      method on_proof_check_res res =
        tick();
        if pp_results then Run_prover_problem.pp_check_result_progress ~w_prover ~w_pb res;
        pp_common_()
      method on_done =
        match cb_progress with
        | None -> ()
        | Some cb -> cb#on_done
    end

  let make ?cb_progress ?(pp_results=true) ?(dyn=false) (r:Exec_run_provers.expanded) : t =
    match cb_progress, pp_results, dyn with
    | None, false, false ->
      nil
    | _ ->
      let len = List.length r.problems in
      let w_prover =
        List.fold_left (fun m p -> max m (String.length (Prover.name p)+1)) 0
          r.provers
        |> min 25
      and w_pb =
        List.fold_left (fun m pb -> max m (String.length pb.Problem.name+1)) 0 r.problems
        |> min 60
      in
      progress ~w_prover ~w_pb ?cb_progress ~pp_results
        ~dyn (len * List.length r.provers)
end

let dump_results_sqlite (results:Test_top_result.t) : unit =
  let uuid = results.Test_top_result.meta.uuid in
  (* save results *)
  let dump_file =
    let filename =
      Printf.sprintf "res-%s-%s.sqlite"
        (CCOpt.map_or ~default:"date"
           Misc.human_datetime results.Test_top_result.meta.timestamp)
        (Uuidm.to_string uuid)
    in
    let data_dir = Filename.concat (Xdg.data_dir ()) !(Xdg.name_of_project) in
    (try Unix.mkdir data_dir 0o744 with _ -> ());
    Filename.concat data_dir filename
  in
  Logs.app (fun k->k "write results into sqlite DB `%s`" dump_file);
  (try
     Db.with_db ~timeout:500 dump_file
             (fun db -> Test_top_result.to_db db results)
   with
   | Error.E e ->
     Logs.err (fun k->k"error when saving to %s:@ %a" dump_file Error.pp e);
     exit 1
   | e ->
     Logs.err (fun k->k"error when saving to %s:@ %s"
                  dump_file (Printexc.to_string e));
     exit 1
  );
  ()

let with_chdir d f =
  let cur_dir = Sys.getcwd () in
  Sys.chdir d;
  try
    let x=f() in
    Sys.chdir cur_dir;
    x
  with e ->
    Sys.chdir cur_dir;
    raise e

let run_cmd ?loc s : unit =
  let c = Sys.command s in
  if c=0 then ()
  else Error.failf ?loc "command %S returned with error code %d" s c

module Git_checkout = struct
  type t = Action.git_checkout

  let run (self:t) : unit =
    Error.guard (Error.wrapf "running action git-checkout '%s'" self.ref) @@ fun () ->
    let {Action.dir; ref; fetch_first; loc=_} = self in
    begin match fetch_first with
      | Some Git_fetch -> run_cmd "git fetch"
      | Some Git_pull -> run_cmd "git pull --ff-only"
      | _ -> ()
    end;
    with_chdir dir
      (fun () -> run_cmd ("git checkout " ^ ref))
end

(** Run the given action *)
let rec run ?(save=true) ?interrupted ?cb_progress
    (defs:Definitions.t) (a:Action.t) : unit =
  Error.guard (Error.wrapf "running action %a" Action.pp a) @@ fun () ->
  begin match a with
    | Action.Act_run_provers r ->
      let is_dyn = CCOpt.get_or ~default:false @@ Definitions.option_progress defs in
      let r_expanded =
        Exec_run_provers.expand ?interrupted ~dyn:is_dyn
          ?j:(Definitions.option_j defs) defs r
      in
      let progress =
        Progress_run_provers.make ~pp_results:true ~dyn:is_dyn
          ?cb_progress r_expanded
      in
      let uuid = Misc.mk_uuid () in
      let res =
        Exec_run_provers.run ?interrupted
          ~on_solve:progress#on_res
          ~on_proof_check:progress#on_proof_check_res
          ~on_done:(fun _ -> progress#on_done) ~save
          ~timestamp:(Misc.now_s()) ~uuid r_expanded
      in
      Format.printf "task done: %a@." Test_compact_result.pp res;
      ()
    | Action.Act_progn l ->
      List.iter (fun a -> run ~save ?interrupted defs a) l
    | Action.Act_git_checkout git ->
      Git_checkout.run git
    | Action.Act_run_cmd {cmd; loc} ->
      run_cmd ~loc cmd
  end


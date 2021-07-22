
(** {1 Execute actions} *)

open Misc

(** File for results with given uuid and timestamp *)
let db_file_for_uuid ~timestamp (uuid:Uuidm.t) : string =
  let filename =
    Printf.sprintf "res-%s-%s.sqlite"
      (match Ptime.of_float_s timestamp with
       | None -> Printf.sprintf "<time %.1fs>" timestamp
       | Some t -> datetime_compact t)
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
    limits : Limit.All.t;
  }

  val expand :
    ?j:int ->
    ?dyn:bool ->
    ?limits:Limit.All.t ->
    ?interrupted:(unit -> bool) ->
    t -> expanded or_error

  val run :
    ?timestamp:float ->
    ?on_start:(expanded -> unit) ->
    ?on_solve:(Test.result -> unit) ->
    ?on_done:(Test_compact_result.t -> unit) ->
    ?interrupted:(unit -> bool) ->
    uuid:Uuidm.t ->
    save:bool ->
    expanded ->
    (Test_top_result.t lazy_t * Test_compact_result.t) or_error
    (** Run the given prover(s) on the given problem set, obtaining results
        after all the problems have been dealt with.
        @param on_solve called whenever a single problem is solved
        @param on_done called when the whole process is done
    *)
end = struct
  open E.Infix

  type t = Action.run_provers
  let (>?) a b = match a with None -> b | Some x -> x
  let (>??) a b = match a with None -> b | Some _ as x -> x

  type expanded = {
    j: int;
    problems: Problem.t list;
    provers: Prover.t list;
    limits : Limit.All.t;
  }

  let filter_regex_ = function
    | None -> (fun _ -> true)
    | Some re ->
      let re = Re.Perl.compile_pat re in
      (fun path -> Re.execp re path)

  (* turn a subdir into a list of problems *)
  let expand_subdir ?pattern ?(interrupted=fun _->false)
      ~dyn (s:Subdir.t) : Problem.t list or_error =
    try
      let filter1 = filter_regex_ s.Subdir.inside.pattern in
      let filter2 = filter_regex_ pattern in
      let filter s = filter1 s && filter2 s in
      let files =
        CCIO.File.walk_l s.Subdir.path
        |> CCList.filter_map
          (fun (kind,f) ->
             if interrupted() then failwith "interrupted";
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
      |> E.flatten_l
    with e ->
      E.of_exn_trace e |> E.add_ctxf "expand_subdir of_dir %a" Subdir.pp s

  (* Expand options into concrete choices *)
  let expand ?j ?(dyn=false) ?limits ?interrupted (self:t) : expanded or_error =
    let limits = match limits with
      | None -> self.limits
      | Some l -> Limit.All.with_defaults l ~defaults:self.limits
    in
    let j = j >?? self.j >? Misc.guess_cpu_count () in
    E.map_l (expand_subdir ~dyn ?interrupted) self.dirs >>= fun problems ->
    let problems = CCList.flatten problems in
    Ok { j; limits; problems; provers=self.provers; }

  let _nop _ = ()

  let run ?(timestamp=now_s())
      ?(on_start=_nop) ?(on_solve = _nop) ?(on_done = _nop)
      ?(interrupted=fun _->false)
      ~uuid ~save
      (self:expanded) : _ E.t =
    let open E.Infix in
    let start = now_s() in
    (* prepare DB *)
    let db =
      if save then (
        let db_file = db_file_for_uuid ~timestamp uuid in
        Sqlite3.db_open ~mutex:`FULL db_file
      ) else
        Sqlite3.db_open ":memory:"
    in
    begin match Sys.getenv "BENCHPRESS_BUSY_TIMEOUT" with
      | n ->
        (try Ok (int_of_string n)
         with _e ->
          E.fail "BENCHPRESS_BUSY_TIMEOUT must be an integer")
      | exception Not_found -> Ok 3000
    end >>= fun ms ->
    Db.setup_timeout db ~ms;
    Test_top_result.db_prepare db >>= fun () ->
    Test_metadata.to_db db
      {Test_metadata.timestamp=Some timestamp; uuid; total_wall_time=None; n_bad=0;
       n_results=0; dirs=[]; provers=[]} >>= fun () ->
    Misc.err_with ~map_err:(Printf.sprintf "while inserting provers: %s")
      (fun scope -> List.iter (fun p -> Prover.to_db db p |> scope.unwrap) self.provers)
    >>= fun () ->
    on_start self;
    (* build list of tasks *)
    let jobs =
      CCList.flat_map
        (fun pb -> CCList.map (fun prover -> prover,pb) self.provers)
        self.problems
    in
    (* run provers *)
    begin
      let db = CCLock.create db in
      Misc.Par_map.map_p ~j:self.j
        (fun (prover,pb) ->
           if interrupted() then E.fail "interrupted"
           else (
             begin
               Run_prover_problem.run ~limits:self.limits
                 prover pb >>= fun result ->
               (* insert into DB here *)
               CCLock.with_lock db (fun db ->
                   Run_event.to_db db (Run_event.mk_prover result))
               >|= fun () ->
               on_solve result; (* callback *)
               result
             end
             |> E.add_ctxf "(@[running :prover %a :on %a@])"
               Prover.pp_name prover Problem.pp pb)
        )
        jobs
      |> E.flatten_l
    end
    >>= fun res_l ->
    if interrupted() then (
      Error "interrupted"
    ) else (
      let total_wall_time = now_s() -. start in
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
      Test_metadata.to_db db meta >>= fun () ->
      let top_res = lazy (
        let provers = CCList.map fst jobs in
        Test_top_result.make ~meta ~provers res_l
        |> E.get_or_failwith
      ) in
      Test_compact_result.of_db db >>= fun r ->
      on_done r;
      Logs.debug (fun k->k "closing db…");
      ignore (Sqlite3.db_close db : bool);
      Ok (top_res, r)
    )
end

type cb_progress = <
  on_progress: percent:int -> elapsed_time:float -> eta:float -> unit;
  on_done: unit;
>

module Progress_run_provers : sig
  type t = <
    on_res: Run_prover_problem.job_res -> unit;
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
    on_done: unit;
  >

  let nil = object method on_res _=() method on_done=() end

  (* callback that prints a result *)
  let progress_dynamic len =
    let start = now_s() in
    let count = ref 0 in
    let tick() = incr count in
    let get_state() =
      let time_elapsed = now_s() -. start in
      let percent = if len=0 then 100. else (float_of_int !count *. 100.) /. float_of_int len in
      (* elapsed=(percent/100)*total, so total=elapsed*100/percent; eta=total-elapsed *)
      let eta = time_elapsed *. (100. -. percent) /. percent in
      percent, time_elapsed, eta
    in
    let pp_bar _ =
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
    object
      method on_res res =
        tick();
        if pp_results then Run_prover_problem.pp_result_progress ~w_prover ~w_pb res;
        if dyn then (
          output_string stdout Misc.reset_line;
          pp_bar res;
        );
        CCOpt.iter
          (fun cb ->
            let percent, elapsed_time, eta = get_state() in
            cb#on_progress ~percent:(int_of_float percent) ~elapsed_time ~eta)
          cb_progress;
        ()
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
     match Db.with_db ~timeout:500 dump_file
             (fun db -> Test_top_result.to_db db results)
     with
     | Ok () -> ()
     | Error e ->
       Logs.err (fun k->k"error when saving to %s:@ %s" dump_file e);
   with e ->
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

let run_cmd s : unit or_error =
  try
    let c = Sys.command s in
    if c=0 then Ok ()
    else Error (Printf.sprintf "command %S returned with error code %d" s c)
  with e ->
    E.of_exn_trace e

module Git_checkout = struct
  type t = Action.git_checkout

  let run (self:t) : unit or_error =
    Misc.err_with
      ~map_err:(Printf.sprintf "while running action git-checkout: %s")
      (fun scope ->
         let {Action.dir; ref; fetch_first} = self in
         begin match fetch_first with
           | Some Git_fetch -> run_cmd "git fetch" |> scope.unwrap
           | Some Git_pull -> run_cmd "git pull --ff-only" |> scope.unwrap
           | _ -> ()
         end;
         with_chdir dir
           (fun () -> run_cmd ("git checkout " ^ ref))
         |> scope.unwrap)
end

(** Run the given action *)
let rec run ?(save=true) ?interrupted ?cb_progress
    (defs:Definitions.t) (a:Action.t) : unit or_error =
  Misc.err_with
    ~map_err:(fun e -> Printf.sprintf "while running action: %s" e)
    (fun scope ->
       begin match a with
         | Action.Act_run_provers r ->
           let is_dyn = CCOpt.get_or ~default:false @@ Definitions.option_progress defs in
           let r_expanded =
             Exec_run_provers.expand ?interrupted ~dyn:is_dyn
               ?j:(Definitions.option_j defs) r
             |> scope.unwrap
           in
           let progress =
             Progress_run_provers.make ~pp_results:true ~dyn:is_dyn
               ?cb_progress r_expanded
           in
           let uuid = Misc.mk_uuid () in
           let res =
             Exec_run_provers.run ?interrupted ~on_solve:progress#on_res
               ~on_done:(fun _ -> progress#on_done) ~save
               ~timestamp:(now_s()) ~uuid r_expanded
             |> scope.unwrap
           in
           Format.printf "task done: %a@." Test_compact_result.pp res;
           ()
         | Action.Act_progn l ->
           List.iter (fun a -> run ~save ?interrupted defs a |> scope.unwrap) l
         | Action.Act_git_checkout git ->
           Git_checkout.run git |> scope.unwrap
         | Action.Act_run_cmd s ->
           (try
              let c = Sys.command s in
              if c=0 then Ok ()
              else Error (Printf.sprintf "command %S returned with error code %d" s c)
            with e ->
              E.of_exn_trace e) |> scope.unwrap
       end
    )


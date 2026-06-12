(** Structured progress tracking.

    A {!t} is a handler that receives events from a running job (solve results,
    proof check results, completion) and forwards them as {!report} values to
    callbacks.

    Multiple handlers can be composed with {!fanout}. *)

open Common
module Api = Benchpress_api_proto.Benchpress_api

type report = Api.progress_report

class type t = object
  method on_res : Run_prover_problem.job_res -> unit
  method on_start_proof_check : unit
  method on_proof_check_res : Test.proof_check_result -> unit
  method on_done : unit
end

type callbacks = {
  on_report: report -> unit;
  on_done: unit -> unit;
  on_res: Run_prover_problem.job_res -> unit;
}

type summary = { percent: int; eta: float }

let summarize (r : report) : summary =
  let total = Int32.to_int r.Api.total_tasks in
  let done_ = Int32.to_int r.Api.done_tasks in
  let percent =
    if total > 0 then
      done_ * 100 / total
    else
      0
  in
  let eta =
    if total = 0 || done_ = 0 then
      infinity
    else (
      let elapsed = Unix.gettimeofday () -. r.Api.start_ts in
      let done_ratio = float_of_int done_ /. float_of_int total in
      if done_ratio = 0. then
        infinity
      else
        elapsed *. (1. -. done_ratio) /. done_ratio
    )
  in
  { percent; eta }

class nil : t =
  object
    method on_res _ = ()
    method on_start_proof_check = ()
    method on_proof_check_res _ = ()
    method on_done = ()
  end

let fanout (l : t list) : t =
  object
    method on_res res = List.iter (fun t -> t#on_res res) l
    method on_start_proof_check = List.iter (fun t -> t#on_start_proof_check) l
    method on_proof_check_res r = List.iter (fun t -> t#on_proof_check_res r) l
    method on_done = List.iter (fun t -> t#on_done) l
  end

(** Internal state for a single job's progress tracking. *)
let make_state ~uuid ~start_ts ~total_tasks =
  let done_tasks = ref 0 in
  let active_items : (float * Api.active_item) list ref = ref [] in
  let active_mutex = Mutex.create () in
  let stat_total_sat = ref 0 in
  let stat_total_unsat = ref 0 in
  let stat_total_unknown = ref 0 in
  let stat_total_timeout = ref 0 in
  let stat_total_error = ref 0 in
  let stat_total_bad = ref 0 in
  let stat_total_custom : (string, int ref) Hashtbl.t = Hashtbl.create 8 in
  let finished = ref false in
  let make_stats () =
    let buf = Buffer.create 128 in
    Buffer.add_string buf
      (spf "sat:%d unsat:%d unknown:%d timeout:%d error:%d bad:%d"
         !stat_total_sat !stat_total_unsat !stat_total_unknown
         !stat_total_timeout !stat_total_error !stat_total_bad);
    Hashtbl.iter
      (fun tag cnt -> Buffer.add_string buf (spf " %s:%d" tag !cnt))
      stat_total_custom;
    Buffer.contents buf
  in
  let add_active (item : Api.active_item) =
    Mutex.lock active_mutex;
    active_items := (Unix.gettimeofday (), item) :: !active_items;
    Mutex.unlock active_mutex
  in
  let snapshot_active () =
    Mutex.lock active_mutex;
    let items =
      List.rev_map (fun (_, item) -> item) !active_items |> CCList.take 10
    in
    active_items := [];
    Mutex.unlock active_mutex;
    items
  in
  let build_report () =
    let r = Api.default_progress_report () in
    Api.progress_report_set_uuid r uuid;
    Api.progress_report_set_start_ts r start_ts;
    Api.progress_report_set_total_tasks r (Int32.of_int total_tasks);
    Api.progress_report_set_done_tasks r (Int32.of_int !done_tasks);
    Api.progress_report_set_active r (snapshot_active ());
    Api.progress_report_set_finished r !finished;
    Api.progress_report_set_stats r (make_stats ());
    let stat_l =
      let add name n acc =
        Api.make_stat ~name ~value:(Int32.of_int n) () :: acc
      in
      [] |> add "sat" !stat_total_sat
      |> add "unsat" !stat_total_unsat
      |> add "unknown" !stat_total_unknown
      |> add "timeout" !stat_total_timeout
      |> add "error" !stat_total_error
      |> add "bad" !stat_total_bad
      |> fun l ->
      Hashtbl.fold (fun tag cnt acc -> add tag !cnt acc) stat_total_custom l
      |> List.rev
    in
    Api.progress_report_set_stat_l r stat_l;
    r
  in
  let bump_stat = function
    | Res.Sat -> incr stat_total_sat
    | Res.Unsat -> incr stat_total_unsat
    | Res.Unknown -> incr stat_total_unknown
    | Res.Timeout -> incr stat_total_timeout
    | Res.Error -> incr stat_total_error
    | Res.Tag tag ->
      (match Hashtbl.find_opt stat_total_custom tag with
      | Some r -> incr r
      | None ->
        let r = ref 0 in
        incr r;
        Hashtbl.add stat_total_custom tag r)
  in
  done_tasks, add_active, bump_stat, build_report, finished, stat_total_bad

let make ~uuid ~start_ts ~total_tasks ~(callbacks : callbacks) : t =
  let done_tasks, add_active, bump_stat, build_report, finished, stat_total_bad
      =
    make_state ~uuid ~start_ts ~total_tasks
  in
  let emit () =
    let r = build_report () in
    callbacks.on_report r
  in
  object
    method on_res (res : Run_prover_problem.job_res) =
      incr done_tasks;
      bump_stat res.Run_result.res;
      if Run_prover_problem.is_bad res then incr stat_total_bad;
      let prover = Run_result.program res in
      let file = (Run_result.problem res).Problem.name in
      let running_time = res.Run_result.raw.Run_proc_result.rtime in
      let item = Api.make_active_item ~prover ~file ~running_time () in
      add_active item;
      callbacks.on_res res;
      emit ()

    method on_start_proof_check = emit ()
    method on_proof_check_res (_res : Test.proof_check_result) = emit ()

    method on_done =
      finished := true;
      emit ();
      callbacks.on_done ()
  end

let make_terminal ~total_tasks ?(pp_results = true) () : t =
  let n_fail = ref 0 in
  let start_time = Misc.now_s () in
  let len = ref total_tasks in
  let count = ref 0 in
  let tick () = incr count in
  let bump () = incr len in
  let get_state () =
    let time_elapsed = Misc.now_s () -. start_time in
    let percent =
      if !len = 0 then
        100.
      else
        float_of_int !count *. 100. /. float_of_int !len
    in
    let eta =
      if percent = 0. then
        infinity
      else
        time_elapsed *. (100. -. percent) /. percent
    in
    percent, time_elapsed, eta
  in
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
    let fail_indicator =
      if !n_fail = 0 then
        ""
      else
        spf " !%d" !n_fail
    in
    Format.printf "... %6d/%d%s | %3.1f%% [%6s: %s] [eta %6s]@?" !count !len
      fail_indicator percent
      (Misc.human_duration time_elapsed)
      bar
      (if eta = infinity then
         "  --"
       else
         Misc.human_duration eta);
    if !count = !len then Format.printf "@."
  in
  let pp_res ~w_prover ~w_pb res =
    if pp_results then Run_prover_problem.pp_result_progress ~w_prover ~w_pb res
  in
  let pp_check_res ~w_prover ~w_pb res =
    if pp_results then
      Run_prover_problem.pp_check_result_progress ~w_prover ~w_pb res
  in
  object
    method on_res res =
      tick ();
      if Run_prover_problem.is_bad res then incr n_fail;
      Misc.synchronized_sync (fun () ->
          output_string stdout Misc.reset_line;
          pp_bar ());
      pp_res ~w_prover:25 ~w_pb:60 res

    method on_start_proof_check =
      bump ();
      Misc.synchronized_sync (fun () ->
          output_string stdout Misc.reset_line;
          pp_bar ())

    method on_proof_check_res res = pp_check_res ~w_prover:25 ~w_pb:60 res

    method on_done =
      Misc.synchronized_sync (fun () ->
          output_string stdout Misc.reset_line;
          pp_bar ())
  end

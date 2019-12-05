open Logitest

module T = Test
module E = CCResult
type 'a or_error = ('a, string) E.t

(* callback that prints a result *)
let progress_dynamic len =
  let start = Unix.gettimeofday () in
  let count = ref 0 in
  fun _ ->
    let time_elapsed = Unix.gettimeofday () -. start in
    incr count;
    let len_bar = 50 in
    let bar = String.init len_bar
        (fun i -> if i * len <= len_bar * !count then '#' else '-') in
    let percent = if len=0 then 100. else (float_of_int !count *. 100.) /. float_of_int len in
    (* elapsed=(percent/100)*total, so total=elapsed*100/percent; eta=total-elapsed *)
    let eta = time_elapsed *. (100. -. percent) /. percent in
    Misc.synchronized
      (fun () ->
         Format.printf "... %5d/%d | %3.1f%% [%6s: %s] [eta %6s]@?"
           !count len percent (Misc.human_time time_elapsed) bar (Misc.human_time eta));
    if !count = len then (
      Misc.synchronized (fun() -> Format.printf "@.")
    )

let progress ~w_prover ~w_pb ?(dyn=false) n =
  let pp_bar = progress_dynamic n in
  (function res ->
     if dyn then output_string stdout Misc.reset_line;
     Test_run.pp_result ~w_prover ~w_pb res;
     if dyn then pp_bar res;
     ())

(* TODO: just do all the dirs at once *)
(* run provers on the given dir, return a list [prover, dir, results] *)
let test_dir
    ?j ?timestamp ?dyn ?timeout ?memory ?provers ~config ~notify d
  : T.Top_result.t or_error =
  let open E.Infix in
  let dir = d.T.Config.directory in
  begin
    Notify.sendf notify "testing dir `%s`…" dir;
    Problem_run.of_dir dir
      ~filter:(Re.execp (Re.Perl.compile_pat d.T.Config.pattern))
    >>= fun pbs ->
    let len = List.length pbs in
    Notify.sendf notify "run %d tests in %s" len dir;
    let provers = match provers with
      | None -> config.T.Config.provers
      | Some l ->
        List.filter
          (fun p -> List.mem (Prover.name p) l)
          config.T.Config.provers
    in
    let on_solve =
      let w_prover =
        List.fold_left (fun m p -> max m (String.length (Prover.name p)+1)) 0 provers
      and w_pb =
        List.fold_left (fun m pb -> max m (String.length pb+1)) 0 pbs
      in
      progress ~w_prover ~w_pb ?dyn (len * List.length provers) in
    (* solve *)
    let main =
      Test_run.run ?timestamp ?j ?timeout ?memory ~provers
        ~expect:d.T.Config.expect ~on_solve ~config pbs
      |> E.add_ctxf "running %d tests" len
    in
    main
    >>= fun results ->
    Prover.Map_name.iter
      (fun p r ->
         Misc.synchronized (fun () ->
         Format.printf "(@[<2>:prover %s :on %S@ @[<2>:results@ %a@]@])@."
           (Prover.name p) dir T.Analyze.pp r))
      (Lazy.force results.T.analyze);
    E.return results
  end |> E.add_ctxf "running tests in dir `%s`" dir

let main ?j ?dyn ?timeout ?memory ?csv ?provers
    ?meta:_ ?summary ~config ?profile ?dir_file dirs () =
  let open E.Infix in
  let timestamp = Unix.gettimeofday() in
  let notify = Notify.make config in
  (* parse list of files, if need be *)
  let dirs = match dir_file with
    | None -> dirs
    | Some f ->
      let f_lines = CCIO.with_in f CCIO.read_lines_l in
      List.rev_append f_lines dirs
  in
  (* parse config *)
  begin
    Test_run.config_of_config ?profile config dirs
    |> E.add_ctxf "parsing config for files (@[%a@])"
      CCFormat.(list ~sep:(return "@ ") string) dirs
  end
  >>= fun config ->
  (* pick default directory if needed *)
  let problems = config.T.Config.problems in
  (* build problem set (exclude config file!) *)
  E.map_l
    (test_dir ?j ~timestamp ?dyn ?timeout ?memory ?provers ~config ~notify)
    problems
  >>= fun l ->
  Misc.Debug.debugf 1 (fun k->k  "merging %d top results…" (List.length l));
  E.return (T.Top_result.merge_l ~timestamp l)
  >>= fun (results:T.Top_result.t) ->
  Utils.dump_csv ~csv results;
  Utils.dump_summary ~summary results;
  Utils.dump_results_json ~timestamp results;
  (* now fail if results were bad *)
  let r = Utils.check_res notify results in
  Notify.sync notify;
  Utils.printbox_results results;
  (* try to send a desktop notification *)
  (try CCUnix.call "notify-send 'benchmark done (%s)'"
         (Misc.human_time results.T.total_wall_time) |> ignore
   with _ -> ());
  r

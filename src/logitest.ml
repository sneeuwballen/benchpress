
(* This file is free software. See file "license" for more details. *)

(* run tests, or compare results *)

module T = Test
module E = CCResult

type 'a or_error = ('a, string) E.t

(** {2 Run} *)
module Run = struct
  (* callback that prints a result *)
  let nb_sec_minute = 60
  let nb_sec_hour = 60 * nb_sec_minute
  let nb_sec_day = 24 * nb_sec_hour

  let time_string f =
    let n = int_of_float f in
    let aux n div = n / div, n mod div in
    let n_day, n = aux n nb_sec_day in
    let n_hour, n = aux n nb_sec_hour in
    let n_min, n = aux n nb_sec_minute in
    let print_aux s n = if n <> 0 then (string_of_int n) ^ s else "" in
    (print_aux "d" n_day) ^
    (print_aux "h" n_hour) ^
    (print_aux "m" n_min) ^
    (string_of_int n) ^ "s"

  let progress_dynamic len =
    let start = Unix.gettimeofday () in
    let count = ref 0 in
    function _ ->
      let time_elapsed = Unix.gettimeofday () -. start in
      incr count;
      let len_bar = 50 in
      let bar = String.init len_bar
          (fun i -> if i * len <= len_bar * !count then '#' else '-') in
      let percent = if len=0 then 100 else (!count * 100) / len in
      Format.printf "... %5d/%d | %3d%% [%6s: %s]@?"
        !count len percent (time_string time_elapsed) bar;
      if !count = len then Format.printf "@."

  let progress ?(dyn=false) n =
    let pp_bar = progress_dynamic n in
    (function res ->
       if dyn then Format.printf "\r";
       Test_run.pp_result res;
       if dyn then pp_bar res;
       ())

  (* run provers on the given dir, return a list [prover, dir, results] *)
  let test_dir ?dyn ?timeout ?memory ?provers ~config d : T.Top_result.t or_error =
    let open E.Infix in
    let dir = d.T.Config.directory in
    begin
      Format.printf "testing dir `%s`...@." dir;
      Problem_run.of_dir dir
        ~filter:(Re.execp (Re_perl.compile_pat d.T.Config.pattern))
      >>= fun pbs ->
      let len = List.length pbs in
      Format.printf "run %d tests in %s@." len dir;
      let provers = match provers with
        | None -> config.T.Config.provers
        | Some l ->
          List.filter
            (fun p -> List.mem (Prover.name p) l)
            config.T.Config.provers
      in
      let on_solve = progress ?dyn (len * List.length provers) in
      (* solve *)
      let main =
        Test_run.run ?timeout ?memory ~provers
          ~expect:d.T.Config.expect ~on_solve ~config pbs
        |> E.add_ctxf "running %d tests" len
      in
      main
      >>= fun results ->
      Prover.Map_name.iter
        (fun p r ->
           Format.printf "@[<2>%s on `%s`:@ @[<hv>%a@]@]@."
             (Prover.name p) dir T.Analyze.pp r)
        (Lazy.force results.T.analyze);
      E.return results
    end |> E.add_ctxf "running tests in dir `%s`" dir

  let check_res (results:T.top_result) : unit or_error =
    let lazy map = results.T.analyze in
    if Prover.Map_name.for_all (fun _ r -> T.Analyze.is_ok r) map
    then E.return ()
    else
      E.fail_fprintf "%d failure(s)"
        (Prover.Map_name.fold (fun _ r n -> n + T.Analyze.num_failed r) map 0)

  (* lwt main *)
  let main ?dyn ?timeout ?memory ?junit ?csv ?provers ?meta:_ ~config ?profile ?dir_file dirs () =
    let open E.Infix in
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
      (test_dir ?dyn ?timeout ?memory ?provers ~config)
      problems
    >>= fun l ->
    Misc.Debug.debugf 1 (fun k->k  "merging %d top results…" (List.length l));
    E.return (T.Top_result.merge_l l)
    >>= fun (results:T.Top_result.t) ->
    (* junit output *)
    begin match junit with
      | None -> ()
      | Some file ->
        Misc.Debug.debugf 1 (fun k->k "write results in Junit to file `%s`" file);
        let suites =
          Lazy.force results.T.analyze
          |> Prover.Map_name.to_list
          |> List.map (fun (_,a) -> JUnit_wrapper.test_analyze a) in
        JUnit_wrapper.junit_to_file suites file;
    end;
    (* CSV output *)
    begin match csv with
      | None ->  ()
      | Some file ->
        Misc.Debug.debugf 1 (fun k->k "write results in CSV to file `%s`" file);
        T.Top_result.to_csv_file file results
    end;
    (* now fail if results were bad *)
    check_res results
end

(** {2 Sample} *)
module Sample = struct
  open E.Infix

  let run ~n dirs =
    E.map_l
      (fun d -> Problem_run.of_dir ~filter:(fun _ -> true) d)
      dirs
    >|= List.flatten
    >|= Array.of_list
    >>= fun files ->
    let len = Array.length files in
    begin
      if len < n
      then E.fail_fprintf "not enough files (need %d, got %d)" n len
      else E.return ()
    end
    >>= fun () ->
    (* sample the list *)
    let sample_idx =
      CCRandom.sample_without_replacement
        ~compare:CCInt.compare n (CCRandom.int len)
      |> CCRandom.run ?st:None
    in
    let sample = List.map (Array.get files) sample_idx in
    (* print sample *)
    List.iter (Printf.printf "%s\n%!") sample;
    E.return ()
end

(** {2 Main: Parse CLI} *)

let config_term =
  let open Cmdliner in
  let aux config debug =
    if debug then (
      Misc.Debug.set_level 5;
    );
    let config = Config.interpolate_home config in
    begin match Config.parse_file config with
      | Result.Ok x -> `Ok x
      | Result.Error e -> `Error (false, e)
    end
  in
  let arg =
    Arg.(value & opt string "~/.logitest.toml" &
         info ["c"; "config"] ~doc:"configuration file (in target directory)")
  and debug =
    let doc = "Enable debug (verbose) output" in
    Arg.(value & flag & info ["d"; "debug"] ~doc)
  in
  Term.(ret (pure aux $ arg $ debug))

(* sub-command for running tests *)
let term_run =
  let open Cmdliner in
  let aux dyn dirs dir_file config profile timeout memory
      meta provers junit =
    Run.main ~dyn ?timeout ?memory ?junit ?provers
      ~meta ?profile ~config ?dir_file dirs ()
  in
  let config = config_term
  and dyn =
    Arg.(value & flag & info ["progress"] ~doc:"print progress bar")
  and dir_file =
    Arg.(value & opt (some string) None & info ["F"] ~doc:"file containing a list of files")
  and profile =
    Arg.(value & opt (some string) None & info ["profile"] ~doc:"pick test profile (default 'test')")
  and timeout =
    Arg.(value & opt (some int) None & info ["t"; "timeout"] ~doc:"timeout (in s)")
  and memory =
    Arg.(value & opt (some int) None & info ["m"; "memory"] ~doc:"memory (in MB)")
  and meta =
    Arg.(value & opt string "" & info ["meta"] ~doc:"additional metadata to save")
  and doc =
    "test a program on every file in a directory"
  and junit =
    Arg.(value & opt (some string) None & info ["junit"] ~doc:"junit output file")
  and dir =
    Arg.(value & pos_all string [] &
         info [] ~docv:"DIR" ~doc:"target directories (containing tests)")
  and provers =
    Arg.(value & opt (some (list string)) None & info ["p"; "provers"] ~doc:"select provers")
  in
  Term.(pure aux $ dyn $ dir $ dir_file $ config $ profile $ timeout $ memory
    $ meta $ provers $ junit),
  Term.info ~doc "run"

let snapshot_name_term : string option Cmdliner.Term.t =
  let open Cmdliner in
  Arg.(value & pos 0 (some string) None
       & info [] ~docv:"FILE" ~doc:"file/name containing results (default: last)")

(* sub-command to sample a directory *)
let term_sample =
  let open Cmdliner in
  let aux n dir = Sample.run ~n dir in
  let dir =
    Arg.(value & pos_all string [] &
         info [] ~docv:"DIR" ~doc:"target directories (containing tests)")
  and n =
    Arg.(value & opt int 1 & info ["n"] ~docv:"N" ~doc:"number of files to sample")
  and doc = "sample N files in the directories" in
  Term.(pure aux $ n $ dir), Term.info ~doc "sample"

let parse_opt () =
  let open Cmdliner in
  let help =
    let doc = "Offers various utilities to test automated theorem provers." in
    let man = [
      `S "DESCRIPTION";
      `P "$(b,logitest) is a set of utils to run tests and compare different \
          results obtained with distinct versions of the same tool";
      `S "COMMANDS";
      `S "OPTIONS"; (* TODO: explain config file *)
    ] in
    Term.(ret (pure (fun () -> `Help (`Pager, None)) $ pure ())),
    Term.info ~version:"dev" ~man ~doc "logitest"
  in
  Cmdliner.Term.eval_choice help [ term_run; term_sample; ]

let () =
  CCFormat.set_color_default true;
  match parse_opt () with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok (Ok ()) -> ()
  | `Ok (Error e) ->
      print_endline ("error: " ^ e);
      exit 1

(* This file is free software. See file "license" for more details. *)

(* run tests, or compare results *)
open Logitest

module T = Test
module E = CCResult

type 'a or_error = ('a, string) E.t

let printbox_results (results:T.top_result) : unit =
  let lazy map = results.T.analyze in
  let box =
    let open PrintBox in
    Prover.Map_name.to_list map
    |> List.map (fun (p,r) -> hlist [hpad 1 @@ text p.Prover.name; T.Analyze.to_printbox r])
    |> vlist |> frame
  in
  Printf.printf "%s\n%!" (PrintBox_text.to_string box);
  ()

let config_term =
  let open Cmdliner in
  let aux config debug =
    if debug then (
      Misc.Debug.set_level 5;
    );
    let default_conf = "$HOME/.logitest.toml" in
    let conf_files = match config with None -> [] | Some c -> [c] in
    let conf_files =
      if Sys.file_exists (Config.interpolate_home default_conf)
      then conf_files @ [default_conf] else conf_files
    in
    let conf_files = List.map Config.interpolate_home conf_files in
    Misc.Debug.debugf 1 (fun k->k "parse config files %a" CCFormat.Dump.(list string) conf_files);
    begin match Config.parse_files conf_files with
      | Result.Ok x -> `Ok x
      | Result.Error e -> `Error (false, e)
    end
  in
  let arg =
    Arg.(value & opt (some string) None &
         info ["c"; "config"] ~doc:"configuration file (in target directory)")
  and debug =
    let doc = "Enable debug (verbose) output" in
    Arg.(value & flag & info ["d"; "debug"] ~doc)
  in
  Term.(ret (pure aux $ arg $ debug))

let snapshot_name_term : string option Cmdliner.Term.t =
  let open Cmdliner in
  Arg.(value & pos 0 (some string) None
       & info [] ~docv:"FILE" ~doc:"file/name containing results (default: last)")

(* CSV output *)
let dump_csv ~csv results : unit =
  begin match csv with
    | None -> ()
    | Some file ->
      Misc.Debug.debugf 1 (fun k->k "write results in CSV to file `%s`" file);
      T.Top_result.to_csv_file file results;
      (try ignore (Sys.command (Printf.sprintf "gzip '%s'" file):int) with _ -> ())
  end

let dump_summary ~summary results : unit =
  (* write summary in some file *)
  begin match summary with
    | None -> ()
    | Some file ->
      CCIO.with_out file
        (fun oc ->
           let out = Format.formatter_of_out_channel oc in
           Format.fprintf out "%a@." T.Top_result.pp_compact results);
  end

let dump_results_json ~timestamp results : unit =
  (* save results *)
  let dump_file =
    let filename =
      Printf.sprintf "res-%s-%s.json"
        (ISO8601.Permissive.string_of_datetime_basic timestamp)
        (Uuidm.v4_gen (Random.State.make_self_init()) () |> Uuidm.to_string)
    in
    let data_dir = Filename.concat (Xdg.data_dir ()) !(Xdg.name_of_project) in
    (try Unix.mkdir data_dir 0o744 with _ -> ());
    Filename.concat data_dir filename
  in
  Misc.Debug.debugf 1 (fun k->k "write results in json to file `%s`" dump_file);
  (try
    CCIO.with_out ~flags:[Open_creat; Open_text] dump_file
      (fun oc ->
         let j = Misc.Json.Encode.encode_value T.Top_result.encode results in
         Misc.Json.J.to_channel oc j; flush oc);
    (* try to compress results *)
    ignore (Sys.command (Printf.sprintf "gzip '%s'" dump_file) : int);
   with e ->
     Printf.eprintf "error when saving to %s: %s\n%!"
       dump_file (Printexc.to_string e);
  );
  ()

let check_res notify (results:T.top_result) : unit or_error =
  let lazy map = results.T.analyze in
  if Prover.Map_name.for_all (fun _ r -> T.Analyze.is_ok r) map
  then (
    Notify.send notify "OK";
    E.return ()
  ) else (
    let n_fail =
      Prover.Map_name.fold (fun _ r n -> n + T.Analyze.num_failed r) map 0
    in
    Notify.sendf notify "FAIL (%d failures)" n_fail;
    E.fail_fprintf "FAIL (%d failures)" n_fail
  )

(** {2 Run} *)
module Run = struct
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
    dump_csv ~csv results;
    dump_summary ~summary results;
    dump_results_json ~timestamp results;
    (* now fail if results were bad *)
    let r = check_res notify results in
    Notify.sync notify;
    printbox_results results;
    (* try to send a desktop notification *)
    (try CCUnix.call "notify-send 'benchmark done (%s)'"
           (Misc.human_time results.T.total_wall_time) |> ignore
     with _ -> ());
    r

  (* sub-command for running tests *)
  let cmd =
    let open Cmdliner in
    let aux j dyn dirs dir_file config profile timeout memory
        meta provers csv summary no_color
      : (unit,string) E.t =
      if no_color then CCFormat.set_color_default false;
      main ~dyn ~j ?timeout ?memory ?csv ?provers
        ~meta ?profile ?summary ~config ?dir_file dirs ()
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
    and j =
      Arg.(value & opt int 1 & info ["j"] ~doc:"level of parallelism")
    and memory =
      Arg.(value & opt (some int) None & info ["m"; "memory"] ~doc:"memory (in MB)")
    and meta =
      Arg.(value & opt string "" & info ["meta"] ~doc:"additional metadata to save")
    and doc =
      "test a program on every file in a directory"
    and csv =
      Arg.(value & opt (some string) None & info ["csv"] ~doc:"CSV output file")
    and dir =
      Arg.(value & pos_all string [] &
           info [] ~docv:"DIR" ~doc:"target directories (containing tests)")
    and provers =
      Arg.(value & opt (some (list string)) None & info ["p"; "provers"] ~doc:"select provers")
    and no_color =
      Arg.(value & flag & info ["no-color"; "nc"] ~doc:"disable colored output")
    and summary =
      Arg.(value & opt (some string) None & info ["summary"] ~doc:"write summary in FILE")
    in
    Term.(pure aux $ j $ dyn $ dir $ dir_file $ config $ profile $ timeout $ memory
      $ meta $ provers $ csv $ summary $ no_color),
    Term.info ~doc "run"
end

module List_files = struct
  let main () =
    try
      let data_dir = Filename.concat (Xdg.data_dir()) "logitest" in
      let entries =
        CCIO.File.walk_l data_dir
        |> CCList.filter_map
          (function
            | (`File, s)
              when (Filename.check_suffix s ".json.gz" ||
                    Filename.check_suffix s ".json") ->
              let size = (Unix.stat s).Unix.st_size in
              Some (s,size)
            | _ -> None)
        |> List.sort CCOrd.compare
      in
      List.iter
        (fun (s,size) ->
           Printf.printf "%s (%s)\n" (Filename.basename s) (Misc.human_size size))
        entries;
      Ok ()
    with e ->
      E.of_exn_trace e

  (* sub-command to sample a directory *)
  let cmd =
    let open Cmdliner in
    let doc = "list benchmark files" in
    Term.(pure main $ pure () ), Term.info ~doc "list-files"
end

module Show = struct
  let load_file (f:string) : (T.Top_result.t, _) E.t =
    try
      let dir = Filename.concat (Xdg.data_dir()) "logitest" in
      let file = Filename.concat dir f in
      if not @@ Sys.file_exists file then (
        Error ("cannot find file " ^ f)
      ) else (
        if Filename.check_suffix f ".gz" then (
          (* use [zcat] to decompress *)
          let p = Unix.open_process_in (Printf.sprintf "zcat '%s'" file) in
          let v = Misc.Json.J.from_channel p in
          (* Format.printf "%a@." (Misc.Json.J.pretty_print ?std:None) v; *)
          Misc.Json.Decode.decode_value T.Top_result.decode v
          |> E.map_err Misc.Json.Decode.string_of_error
        ) else (
          Misc.Json.Decode.decode_file T.Top_result.decode file
          |> E.map_err Misc.Json.Decode.string_of_error
        )
      )
    with e ->
      E.of_exn_trace e

  let main ?(check=true) ?(bad=true) ?csv ?summary files =
    let open E.Infix in
    E.map_l load_file files >>= fun res ->
    let results = T.Top_result.merge_l res in
    dump_csv ~csv results;
    dump_summary ~summary results;
    printbox_results results;
    if bad then (
      Format.printf "@[<2>bad: %a@]@." T.Top_result.pp_bad results;
    );
    if check then check_res Notify.nil results else E.return ()


  (* sub-command for showing results *)
  let cmd =
    let open Cmdliner in
    let csv =
      Arg.(value & opt (some string) None & info ["csv"] ~doc:"CSV output file")
    and files =
      Arg.(value & pos_all string [] &
           info [] ~docv:"FILES" ~doc:"files to read")
    and no_color =
      Arg.(value & flag & info ["no-color"; "nc"] ~doc:"disable colored output")
    and check =
      Arg.(value & opt ~vopt:true bool true & info ["check"] ~doc:"check results")
    and bad =
      Arg.(value & opt ~vopt:true bool true & info ["bad"] ~doc:"list bad results")
    and summary =
      Arg.(value & opt (some string) None & info ["summary"] ~doc:"write summary in FILE")
    in
    let aux check bad csv summary no_color files : _ E.t = 
      if no_color then CCFormat.set_color_default false;
      main ~check ~bad ?csv ?summary files
    in
    let doc = "show benchmark results" in
    Term.(pure aux $ check $ bad $ csv $ summary $ no_color $ files),
    Term.info ~doc "show"
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
    Misc.synchronized (fun () -> List.iter (Printf.printf "%s\n%!") sample);
    E.return ()

  (* sub-command to sample a directory *)
  let cmd =
    let open Cmdliner in
    let aux n dir = run ~n dir in
    let dir =
      Arg.(value & pos_all string [] &
           info [] ~docv:"DIR" ~doc:"target directories (containing tests)")
    and n =
      Arg.(value & opt int 1 & info ["n"] ~docv:"N" ~doc:"number of files to sample")
    and doc = "sample N files in the directories" in
    Term.(pure aux $ n $ dir), Term.info ~doc "sample"
end

(** {2 Main: Parse CLI} *)


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
  Cmdliner.Term.eval_choice help [
    Run.cmd;
    Sample.cmd;
    List_files.cmd;
    Show.cmd;
  ]

let () =
  CCFormat.set_color_default true;
  match parse_opt () with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok (Ok ()) -> ()
  | `Ok (Error e) ->
      print_endline ("error: " ^ e);
      exit 1

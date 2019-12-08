open Logitest
module T = Test
module E = CCResult

type 'a or_error = ('a, string) E.t

let default_conf () = 
  let (//) = Filename.concat in
  Xdg.config_dir() // "logitest" // "conf.sexp"

let definitions_term : Definitions.t Cmdliner.Term.t =
  let open Cmdliner in
  let aux config config_toml debug =
    CCOpt.iter Misc.Debug.set_level debug;
    let conf_files = match config with None -> [] | Some c -> [c] in
    let conf_files =
      let default_conf = default_conf () in
      (* always add default config file if it exists *)
      if Sys.file_exists (Xdg.interpolate_home default_conf)
      then default_conf :: conf_files else conf_files
    in
    let conf_files = List.map Xdg.interpolate_home conf_files in
    let toml_files = match config_toml with None -> [] | Some c -> [c] in
    Misc.Debug.debugf 1 (fun k->k "parse config files %a" CCFormat.Dump.(list string) conf_files);
    begin match Stanza.parse_files conf_files, Config.parse_files toml_files with
      | Ok x, Ok y ->
        (* combine configs *)
        Misc.Debug.debugf 4 (fun k->k "combine configs…");
        begin match E.(
          Definitions.of_config y >>= fun defs ->
          Definitions.add_stanza_l ~cur_dir:(Sys.getcwd()) x defs
        ) with
          | Ok x -> `Ok x
          | Error s -> `Error (false, s)
        end
      | Error e, _ | _, Error e -> `Error (false, e)
    end
  in
  let arg_toml =
    Arg.(value & opt (some string) None &
         info ["ct"; "config-toml"] ~doc:"configuration file (toml; in target directory; DEPRECATED)")
  and arg =
    Arg.(value & opt (some string) None &
         info ["c"; "config"] ~doc:"configuration file (sexp)")
  and debug =
    let doc = "Enable debug (verbose) output" in
    Arg.(value & opt (some int) None & info ["d"; "debug"] ~doc)
  in
  Term.(ret (pure aux $ arg $ arg_toml $ debug))

(* CSV output *)
let dump_csv ~csv results : unit =
  begin match csv with
    | None -> ()
    | Some file ->
      Misc.Debug.debugf 1 (fun k->k "write results in CSV to file `%s`" file);
      T.Top_result.to_csv_file file results;
      (try ignore (Sys.command (Printf.sprintf "gzip -f '%s'" file):int) with _ -> ())
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
    ignore (Sys.command (Printf.sprintf "gzip -f '%s'" dump_file) : int);
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

let list_entries data_dir =
  CCIO.File.walk_l data_dir
  |> CCList.filter_map
    (function
      | (`File, s)
        when (Filename.check_suffix s ".json.gz" ||
              Filename.check_suffix s ".json") ->
        let size = (Unix.stat s).Unix.st_size in
        Some (s,size)
      | _ -> None)
  |> List.sort (fun x y->CCOrd.compare y x)

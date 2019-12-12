module T = Test
module E = CCResult
module Db = Misc.Db

type 'a or_error = ('a, string) E.t

let (//) = Filename.concat

let data_dir () = Xdg.data_dir () // !(Xdg.name_of_project)
let config_dir () = Xdg.config_dir () // !(Xdg.name_of_project)
let default_config () = config_dir() // "conf.sexp"

let definitions_term : Definitions.t Cmdliner.Term.t =
  let open Cmdliner in
  let aux config config_toml logs_cmd =
    Misc.setup_logs logs_cmd;
    let conf_files = match config with None -> [] | Some c -> [c] in
    let conf_files =
      let default_conf = default_config () in
      (* always add default config file if it exists *)
      if Sys.file_exists (Xdg.interpolate_home default_conf)
      then default_conf :: conf_files else conf_files
    in
    let conf_files = List.map Xdg.interpolate_home conf_files in
    let toml_files = match config_toml with None -> [] | Some c -> [c] in
    Logs.info (fun k->k "parse config files %a" CCFormat.Dump.(list string) conf_files);
    begin match Stanza.parse_files conf_files, Config.parse_files toml_files with
      | Ok x, Ok y ->
        (* combine configs *)
        Logs.debug (fun k->k "combine configs…");
        begin match E.(
          Definitions.of_config y >>= fun defs ->
          Definitions.add_stanza_l x defs
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
    Logs_cli.level ()
  in
  Term.(ret (pure aux $ arg $ arg_toml $ debug))

(* CSV output *)
let dump_csv ~csv results : unit =
  begin match csv with
    | None -> ()
    | Some file ->
      Logs.app (fun k->k "write results in CSV to file `%s`" file);
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
  Logs.app (fun k->k "write results in json to file `%s`" dump_file);
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
[@@ocaml.deprecated "use sqlite instead"]

let dump_results_sqlite results : unit =
  (* save results *)
  let dump_file =
    (* FIXME: results should have their own UUID already *)
    let filename =
      Printf.sprintf "res-%s-%s.sqlite"
        (ISO8601.Permissive.string_of_datetime_basic results.Test.timestamp)
        (Uuidm.v4_gen (Random.State.make_self_init()) () |> Uuidm.to_string)
    in
    let data_dir = Filename.concat (Xdg.data_dir ()) !(Xdg.name_of_project) in
    (try Unix.mkdir data_dir 0o744 with _ -> ());
    Filename.concat data_dir filename
  in
  Logs.app (fun k->k "write results into sqlite DB `%s`" dump_file);
  (try
     match Db.with_db dump_file
       (fun db -> Test.Top_result.to_db db results)
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
    |> List.map
      (fun (p,r) -> frame @@ hlist [
           center_hv @@ pad @@ text p.Prover.name;
           T.Analyze.to_printbox r])
    |> hlist ~bars:false ~pad:(hpad 1)
  in
  Printf.printf "%s\n%!" (PrintBox_text.to_string box);
  ()

let list_entries data_dir =
  CCIO.File.walk_l data_dir
  |> CCList.filter_map
    (function
      | (`File, s)
        when (Filename.check_suffix s ".json.gz" ||
              Filename.check_suffix s ".json" ||
              Filename.check_suffix s ".sqlite") ->
        let size = (Unix.stat s).Unix.st_size in
        Some (s,size)
      | _ -> None)
  |> List.sort (fun x y->CCOrd.compare y x)

(** Load file by name *)
let load_file_full (f:string) : (string*T.Top_result.t, _) E.t =
  try
    let dir = Filename.concat (Xdg.data_dir()) !(Xdg.name_of_project) in
    let file = Filename.concat dir f in
    if not @@ Sys.file_exists file then (
      Error ("cannot find file " ^ f)
    ) else (
      if Filename.check_suffix f ".sqlite" then (
      ) else if Filename.check_suffix f ".gz" then (
        (* use [zcat] to decompress *)
        let v =
          CCUnix.with_process_in (Printf.sprintf "zcat '%s'" file)
            ~f:Misc.Json.J.from_channel in
        (* Format.printf "%a@." (Misc.Json.J.pretty_print ?std:None) v; *)
        Misc.Json.Decode.decode_value T.Top_result.decode v
        |> E.map_err Misc.Json.Decode.string_of_error
        |> E.map (fun r -> file, r)
      ) else (
        Misc.Json.Decode.decode_file T.Top_result.decode file
        |> E.map_err Misc.Json.Decode.string_of_error
        |> E.map (fun r -> file, r)
      )
    )
  with e ->
    E.of_exn_trace e

let load_file f = E.map snd @@ load_file_full f

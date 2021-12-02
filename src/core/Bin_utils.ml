module T = Test
module Db = Misc.Db
module MStr = Misc.Str_map

let definitions_term : Definitions.t Cmdliner.Term.t =
  let open Cmdliner in
  let aux conf_files logs_cmd =
    Misc.setup_logs logs_cmd;
    let conf_files = CCList.flatten conf_files in
    let conf_files =
      let default_conf = Misc.default_config () in
      (* always add default config file if it exists *)
      if Sys.file_exists (Xdg.interpolate_home default_conf)
      then default_conf :: conf_files else conf_files
    in
    let conf_files = List.map Xdg.interpolate_home conf_files in
    Logs.info (fun k->k "parse config files %a" CCFormat.Dump.(list string) conf_files);
    try
      let stanzas = Stanza.parse_files conf_files in
      let defs = Definitions.add_stanza_l stanzas Definitions.empty in
      `Ok defs
    with
    | Error.E err -> `Error (false, Error.show err)
  in
  let args =
    Arg.(value & opt_all (list ~sep:',' string) [] &
         info ["c"; "config"] ~doc:"configuration file (sexp)")
  and debug =
    Logs_cli.level ()
  in
  Term.(ret (pure aux $ args $ debug))

let get_definitions () : Definitions.t =
  let conf_files =
    let default_conf = Misc.default_config () in
    (* always add default config file if it exists *)
    if Sys.file_exists (Xdg.interpolate_home default_conf)
    then [default_conf] else []
  in
  let conf_files = List.map Xdg.interpolate_home conf_files in
  Logs.info (fun k->k "parse config files %a" CCFormat.Dump.(list string) conf_files);
  let l = Stanza.parse_files conf_files in
  (* combine configs *)
  Definitions.of_stanza_l l

(* CSV output *)
let dump_csv ~csv results : unit =
  begin match csv with
    | None -> ()
    | Some file ->
      Logs.app (fun k->k "write results in CSV to file `%s`" file);
      Test_top_result.to_csv_file file results;
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
           Format.fprintf out "%a@." Test_top_result.pp_compact results);
  end

let check_res_an notify a : unit =
  if List.for_all (fun (_,r) -> Test_analyze.is_ok r) a
  then (
    Notify.send notify "OK";
  ) else (
    let n_fail =
      List.fold_left (fun n (_,r) -> n + Test_analyze.num_bad r) 0 a
    in
    Notify.sendf notify "FAIL (%d failures)" n_fail;
    Error.failf "FAIL (%d failures)" n_fail
  )

let check_compact_res notify (results:Test_compact_result.t) : unit =
  check_res_an notify (results.Test_compact_result.cr_analyze)

let check_res notify (results:Test_top_result.t) : unit =
  let a = Test_top_result.analyze results in
  check_res_an notify a

let printbox_stat st : unit =
  let box_st = Test_stat.to_printbox_l st in
  Printf.printf "STAT:\n%s\n%!" (PrintBox_text.to_string box_st);
  ()

let printbox_analyze a =
  let box_a = Test_analyze.to_printbox_l a in
  Printf.printf "ANALYSIS:\n%s\n%!" (PrintBox_text.to_string box_a);
  ()

let printbox_compact_results (results:Test_compact_result.t) : unit =
  printbox_stat results.cr_stat;
  printbox_analyze results.cr_analyze;
  ()

let printbox_results (results:Test_top_result.t) : unit =
  begin
    let st = Test_top_result.stat results in
    printbox_stat st;
  end;
  begin
    let a = Test_top_result.analyze results in
    printbox_analyze a;
  end;
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
  |> List.sort (fun x y->CCOrd.(pair CCString.compare_natural int) y x)

(* find absolute path of [f] in the data dir *)
let mk_file_full (f:string) : string =
  let dir = Filename.concat (Xdg.data_dir()) !(Xdg.name_of_project) in
  let file = Filename.concat dir f in
  if not @@ Sys.file_exists file then (
    Error.failf "cannot find file '%s'" f
  ) else (
    file
  )

let guess_uuid (f:string) =
  let f = Filename.chop_extension @@ Filename.basename f in
  try
    Scanf.sscanf f "res-%[^-%]-%s%!"
      (fun _ s -> Some s)
  with e ->
    Logs.err (fun k->k"cannot find UUID for %s: %s" f(Printexc.to_string e));
    None

(** Load file by name *)
let load_file_full (file:string) : string*Test_top_result.t =
  let file = mk_file_full file in
  if Filename.check_suffix file ".sqlite" then (
    Error.guard (Error.wrapf "load_file_full '%s'" file) @@ fun () ->
    Db.with_db ~timeout:1500 ~mode:`NO_CREATE file
      (fun db ->
         let r = Test_top_result.of_db db in
         file, r)
  ) else (
    Error.failf "invalid name %S, expected a .sqlite file" file
  )

let with_file_as_db ~map_err filename file : _ =
  Error.guard map_err @@ fun () ->
  Error.guard (Error.wrapf "processing DB '%s'" filename) @@ fun () ->
  let filename = mk_file_full filename in try
    Db.with_db ~timeout:500 ~mode:`READONLY filename file
  with
  | Error.E _ as e -> raise e
  | Db.RcError rc -> Error.raise (Misc.err_of_db rc)
  | e -> Error.raise (Error.of_exn e)

let load_file f = snd @@ load_file_full f

let load_file_summary ?(full=false)
    (file:string) : string * Test_compact_result.t =
  if Filename.check_suffix file ".sqlite" then (
    let file = mk_file_full file in
    Db.with_db ~timeout:500 ~mode:`READONLY file
      (fun db ->
         let cr = Test_compact_result.of_db ~full db in
         file, cr)
  ) else (
    let file', res = load_file_full file in
    let cr = Test_top_result.to_compact_result res in
    file', cr
  )

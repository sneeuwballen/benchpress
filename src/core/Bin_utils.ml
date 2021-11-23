module T = Test
module E = CCResult
module Db = Misc.Db
module MStr = Misc.Str_map

type 'a or_error = ('a, string) E.t

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
    begin match Stanza.parse_files conf_files with
      | Ok x ->
        begin match Definitions.add_stanza_l x Definitions.empty with
          | Ok x -> `Ok x
          | Error s -> `Error (false, s)
        end
      | Error (e, _loc) -> `Error (false, e)
    end
  in
  let args =
    Arg.(value & opt_all (list ~sep:',' string) [] &
         info ["c"; "config"] ~doc:"configuration file (sexp)")
  and debug =
    Logs_cli.level ()
  in
  Term.(ret (pure aux $ args $ debug))

let get_definitions () : Definitions.t or_error =
  let conf_files =
    let default_conf = Misc.default_config () in
    (* always add default config file if it exists *)
    if Sys.file_exists (Xdg.interpolate_home default_conf)
    then [default_conf] else []
  in
  let conf_files = List.map Xdg.interpolate_home conf_files in
  Logs.info (fun k->k "parse config files %a" CCFormat.Dump.(list string) conf_files);
  let open E.Infix in
  (Stanza.parse_files conf_files |> E.map_err fst) >>= fun l ->
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

let check_res_an notify a : unit or_error =
  if List.for_all (fun (_,r) -> Test_analyze.is_ok r) a
  then (
    Notify.send notify "OK";
    E.return ()
  ) else (
    let n_fail =
      List.fold_left (fun n (_,r) -> n + Test_analyze.num_bad r) 0 a
    in
    Notify.sendf notify "FAIL (%d failures)" n_fail;
    E.fail_fprintf "FAIL (%d failures)" n_fail
  )

let check_compact_res notify (results:Test_compact_result.t) : unit or_error =
  check_res_an notify (results.Test_compact_result.cr_analyze)

let check_res notify (results:Test_top_result.t) : unit or_error =
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
let mk_file_full (f:string) : string or_error =
  let dir = Filename.concat (Xdg.data_dir()) !(Xdg.name_of_project) in
  let file = Filename.concat dir f in
  if not @@ Sys.file_exists file then (
    Error ("cannot find file " ^ f)
  ) else (
    Ok file
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
let load_file_full (f:string) : (string*Test_top_result.t, _) E.t =
  try
    match mk_file_full f with
    | Error _ as e -> e
    | Ok file ->
      if Filename.check_suffix f ".sqlite" then (
        try
          Db.with_db ~timeout:1500 ~mode:`NO_CREATE file
            (fun db -> Test_top_result.of_db db |> E.map (fun r->file,r))
        with e -> E.of_exn e
      ) else (
        E.fail_fprintf "invalid name %S, expected a .sqlite file" f
      )
  with e ->
    E.of_exn_trace e

let with_file_as_db filename f : _ E.t =
  Misc.err_with
    ~map_err:(fun (e,code) ->
        Printf.sprintf "while processing DB %s: %s" filename e,code)
    (fun scope ->
       let filename =
         mk_file_full filename |> scope.unwrap_with (fun e->e,500) in
       try
         Db.with_db ~timeout:500 ~mode:`READONLY filename
           (fun db -> f (scope, db))
       with
       | Db.RcError rc ->
         scope.unwrap_with (fun c->Db.Rc.to_string c,500) (Error rc)
       | e -> scope.unwrap (Error (Printexc.to_string e, 500)))

let load_file f = E.map snd @@ load_file_full f

let load_file_summary ?(full=false) (f:string) : (string * Test_compact_result.t,_) E.t =
  let open E.Infix in
  if Filename.check_suffix f ".sqlite" then (
    match mk_file_full f with
    | Error _ as e -> e
    | Ok file ->
      Db.with_db ~timeout:500 ~mode:`READONLY file
        (fun db ->
           Test_compact_result.of_db ~full db >>= fun cr ->
           E.return (file, cr))
  ) else (
    load_file_full f >>= fun (f,res) ->
    Test_top_result.to_compact_result res >>= fun cr ->
    E.return (f, cr)
  )

let lift_err = function
  | Ok x -> Ok x
  | Error s -> Error (s, [])


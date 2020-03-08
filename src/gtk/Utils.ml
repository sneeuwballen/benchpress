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
      | Error e -> `Error (false, e)
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
  Stanza.parse_files conf_files >>= fun l ->
  (* combine configs *)
  Definitions.of_stanza_l l

let list_entries data_dir : _ list =
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

let with_file_as_db filename f : _ E.t =
  Misc.err_with
    ~map_err:(Printf.sprintf "while processing DB %s: %s" filename)
    (fun scope ->
       let filename = mk_file_full filename |> scope.unwrap in
       try
         Db.with_db ~timeout:500 ~mode:`READONLY filename
           (fun db -> f scope db)
       with
       | Db.RcError rc -> scope.unwrap_with Db.Rc.to_string (Error rc)
       | e -> scope.unwrap (Error (Printexc.to_string e)))

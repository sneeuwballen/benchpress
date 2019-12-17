(* This file is free software. See file "license" for more details. *)

(* run tests, or compare results *)

module T = Test
module E = CCResult

type 'a or_error = ('a, string) E.t

(** {2 Run} *)
module Run = struct
  (* sub-command for running tests *)
  let cmd =
    let open Cmdliner in
    let aux j dyn paths dir_file defs task timeout memory
        meta provers csv summary no_color
      : (unit,string) E.t =
      if no_color then CCFormat.set_color_default false;
      let dyn = if dyn then Some true else None in
      Run_main.main ?dyn ~j ?timeout ?memory ?csv ?provers
        ~meta ?task ?summary ?dir_file defs paths ()
    in
    let defs = Utils.definitions_term
    and dyn =
      Arg.(value & flag & info ["progress"] ~doc:"print progress bar")
    and dir_file =
      Arg.(value & opt (some string) None & info ["F"] ~doc:"file containing a list of files")
    and task =
      Arg.(value & opt (some string) None & info ["task"] ~doc:"task to run")
    and timeout =
      Arg.(value & opt (some int) None & info ["t"; "timeout"] ~doc:"timeout (in s)")
    and j =
      Arg.(value & opt int 1 & info ["j"] ~doc:"level of parallelism")
    and memory =
      Arg.(value & opt (some int) None & info ["m"; "memory"] ~doc:"memory (in MB)")
    and meta =
      Arg.(value & opt string "" & info ["meta"] ~doc:"additional metadata to save")
    and doc =
      "run a task, such as running solvers on directories of problem files"
    and csv =
      Arg.(value & opt (some string) None & info ["csv"] ~doc:"CSV output file")
    and paths =
      Arg.(value & pos_all string [] &
           info [] ~docv:"PATH" ~doc:"target paths (or directories containing tests)")
    and provers =
      Arg.(value & opt (some (list string)) None & info ["p"; "provers"] ~doc:"select provers")
    and no_color =
      Arg.(value & flag & info ["no-color"; "nc"] ~doc:"disable colored output")
    and summary =
      Arg.(value & opt (some string) None & info ["summary"] ~doc:"write summary in FILE")
    in
    Term.(pure aux $ j $ dyn $ paths $ dir_file $ defs $ task $ timeout $ memory
      $ meta $ provers $ csv $ summary $ no_color),
    Term.info ~doc "run"
end

module List_files = struct
  let main ?(abs=false) () =
    try
      let data_dir = Misc.data_dir() in
      let entries = Utils.list_entries data_dir in
      List.iter
        (fun (s,size) ->
           let s = if abs then s else Filename.basename s in
           Printf.printf "%s (%s)\n" s (Misc.human_size size))
        entries;
      Ok ()
    with e ->
      E.of_exn_trace e

  (* sub-command to sample a directory *)
  let cmd =
    let open Cmdliner in
    let abs =
      Arg.(value & opt ~vopt:true bool false & info ["abs"] ~doc:"show absolute paths")
    in
    let doc = "list benchmark result files" in
    let aux abs () = main ~abs () in
    Term.(pure aux $ abs $ pure () ), Term.info ~doc "list-files"
end

module Show = struct
  (* sub-command for showing results *)
  let cmd =
    let open Cmdliner in
    let csv =
      Arg.(value & opt (some string) None & info ["csv"] ~doc:"CSV output file")
    and file =
      Arg.(required & pos 0 (some string) None &
           info [] ~docv:"FILE" ~doc:"file to read")
    and no_color =
      Arg.(value & flag & info ["no-color"; "nc"] ~doc:"disable colored output")
    and check =
      Arg.(value & opt ~vopt:true bool true & info ["check"] ~doc:"check results")
    and bad =
      Arg.(value & opt ~vopt:true bool true & info ["bad"] ~doc:"list bad results")
    and summary =
      Arg.(value & opt (some string) None & info ["summary"] ~doc:"write summary in FILE")
    and debug =
      Logs_cli.level ()
    in
    let aux check bad csv summary no_color debug file : _ E.t = 
      Misc.setup_logs debug;
      if no_color then CCFormat.set_color_default false;
      Show.main ~check ~bad ?csv ?summary file
    in
    let doc = "show benchmark results (see `list-files`)" in
    Term.(pure aux $ check $ bad $ csv $ summary $ no_color $ debug $ file),
    Term.info ~doc "show"
end

(** {2 Sample} *)
module Sample = struct
  open E.Infix

  let files_of_dir (p:string) : string list or_error =
    try
      CCIO.File.walk_l p
      |> CCList.filter_map
        (fun (kind,f) -> match kind with
           | `File -> Some f
           | _ -> None)
      |> E.return
    with e ->
      E.of_exn_trace e |> E.add_ctxf "expand_subdir of_dir %S" p

  let run ~n dirs =
    E.map_l files_of_dir dirs
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
    and doc = "sample N files in the given directories" in
    Term.(pure aux $ n $ dir), Term.info ~doc "sample"
end

(** {2 Embedded web server} *)

module Serve = struct
  (* sub-command to serve the web UI *)
  let cmd =
    let open Cmdliner in
    let port =
      Arg.(value & opt (some int) None & info ["p";"port"] ~doc:"port to listen on")
    and defs =
      Utils.definitions_term
    in
    let doc = "serve embedded web UI on given port" in
    let aux defs port () =
      Serve.main ?port defs () in
    Term.(pure aux $ defs $ port $ pure () ), Term.info ~doc "serve"
end

(** {2 Show directories} *)

module Dir = struct
  type which = Config | State

  let which_conv = Cmdliner.Arg.(enum ["config", Config; "state", State])

  let run c =
    Format.printf "%s@."
      (match c with
       | Config -> Misc.config_dir()
       | State -> Misc.data_dir ());
    Ok ()

  (* sub-command for showing results *)
  let cmd =
    let open Cmdliner in
    let which =
      Arg.(required & pos 0 (some which_conv) None & info ~doc:"directory to list (config|state)" [])
    in
    let doc = "show directories where benchpress stores its state (config|state)" in
    Term.(pure run $ which),
    Term.info ~doc "dir"

end

(** {2 Check config} *)

module Check_config = struct
  let run with_default f =
    let default_file = Misc.default_config () in
    let f =
      if f=[] then (
        if Sys.file_exists default_file then [default_file] else []
      ) else if with_default && Sys.file_exists default_file then Misc.default_config() :: f else f in
    match Stanza.parse_files f with
    | Ok c ->
      Format.printf "@[<v>%a@]@." Stanza.pp_l c;
      Ok ()
    | Error e -> Error e

  let cmd =
    let open Cmdliner in
    let files =
      Arg.(value & pos_all string [] & info [] ~doc:"file(s) to check")
    and with_default =
      Arg.(value & opt bool false & info ["d"; "default"] ~doc:"combine with the default config file(s)")
    in
    let doc = "parse and print configuration file(s)" in
    let aux with_default files () = run with_default files in
    Term.(pure aux $ with_default $ files $ pure () ), Term.info ~doc "check-config"
end

(** {2 See prover(s)} *)

module Prover_show = struct
  let run defs names =
    let open E.Infix in
    E.map_l (Definitions.find_prover defs) names >>= fun l ->
    Format.printf "@[<v>%a@]@." (Misc.pp_list Prover.pp) l;
    Ok ()

  let cmd =
    let open Cmdliner in
    let doc = "show definitions of given prover(s)" in
    let names = Arg.(value & pos_all string [] & info []) in
    Term.(pure run $ Utils.definitions_term $ names ), Term.info ~doc "prover-show"
end

(** {2 List provers} *)

module Prover_list = struct
  let run defs =
    let l = Definitions.all_provers defs in
    Format.printf "@[<v>%a@]@." (Misc.pp_list Prover.pp_name) l;
    Ok ()

  let cmd =
    let open Cmdliner in
    let doc = "list prover(s) defined in config" in
    Term.(pure run $ Utils.definitions_term), Term.info ~doc "prover-list"
end

(** {2 Show Task} *)

module Task_show = struct
  let run defs names =
    let open E.Infix in
    E.map_l (Definitions.find_task defs) names >>= fun l ->
    Format.printf "@[<v>%a@]@." (Misc.pp_list Task.pp) l;
    Ok ()

  let cmd =
    let open Cmdliner in
    let doc = "show definitions of given task(s)" in
    let names = Arg.(value & pos_all string [] & info []) in
    Term.(pure run $ Utils.definitions_term $ names ), Term.info ~doc "task-show"
end

(** {2 List Tasks} *)

module Task_list = struct
  let run defs =
    let l = Definitions.all_tasks defs in
    Format.printf "@[<v>%a@]@." (Misc.pp_list Task.pp_name) l;
    Ok ()

  let cmd =
    let open Cmdliner in
    let doc = "list task(s) defined in config" in
    Term.(pure run $ Utils.definitions_term), Term.info ~doc "task-list"
end

(** {2 Convert results to Sql} *)

module Sql_convert = struct
  (* sub-command for showing results *)
  let cmd =
    let open Cmdliner in
    let files =
      Arg.(non_empty & pos_all string [] &
           info [] ~docv:"FILES" ~doc:"files to read")
    in
    let doc = "convert result(s) into sqlite files" in
    Term.(pure Sql_res.run $ Utils.definitions_term $ files),
    Term.info ~doc "sql-convert"
end

(** {2 Main: Parse CLI} *)

let parse_opt () =
  let open Cmdliner in
  let help =
    let doc = "Tool to test logic solver and automatic theorem provers." in
    let man = [
      `S "DESCRIPTION";
      `P "$(b,benchpress) is a tool to run tests and compare different \
          results obtained with distinct tools or versions of the same tool";
      `S "COMMANDS";
      `S "OPTIONS"; (* TODO: explain config file *)
    ] in
    Term.(ret (pure (fun () -> `Help (`Pager, None)) $ pure ())),
    Term.info ~version:"dev" ~man ~doc "benchpress"
  in
  Cmdliner.Term.eval_choice help [
    Dir.cmd;
    Run.cmd;
    Sample.cmd;
    List_files.cmd;
    Show.cmd;
    Serve.cmd;
    Check_config.cmd;
    Prover_show.cmd;
    Prover_list.cmd;
    Sql_convert.cmd;
    Task_list.cmd;
    Task_show.cmd;
  ]

let () =
  CCFormat.set_color_default true;
  match parse_opt () with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok (Ok ()) -> ()
  | `Ok (Error e) ->
      print_endline ("error: " ^ e);
      exit 1

(* This file is free software. See file "license" for more details. *)

(* run tests, or compare results *)

open Common
module T = Test
module Db = Sqlite3_utils

let catch_err f =
  try f(); true
  with Error.E e ->
    Format.eprintf "%a@." Error.pp e;
    false

(** {2 Run} *)
module Run = struct
  (* sub-command for running tests *)
  let cmd =
    let open Cmdliner in
    let aux j pp_results dyn paths dir_file proof_dir defs task timeout memory
        meta provers csv summary no_color save =
      catch_err @@ fun () ->
      if no_color then CCFormat.set_color_default false;
      let dyn = if dyn then Some true else None in
      Run_main.main ~pp_results ?dyn ~j ?timeout ?memory ?csv ~provers
        ~meta ?task ?summary ?dir_file ?proof_dir ~save defs paths ()
    in
    let defs = Bin_utils.definitions_term
    and dyn =
      Arg.(value & flag & info ["progress"] ~doc:"print progress bar")
    and pp_results =
      Arg.(value & opt bool true & info ["pp-results"] ~doc:"print results as they are found")
    and save =
      Arg.(value & opt bool true & info ["save"] ~doc:"save results on disk")
    and dir_file =
      Arg.(value & opt (some string) None & info ["F"] ~doc:"file containing a list of files")
    and proof_dir =
      Arg.(value & opt (some string) None & info ["proof-dir"] ~doc:"store proofs in given directory")
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
      Arg.(value & opt_all string [] & info ["p"; "provers"] ~doc:"select provers")
    and no_color =
      Arg.(value & flag & info ["no-color"; "nc"] ~doc:"disable colored output")
    and summary =
      Arg.(value & opt (some string) None & info ["summary"] ~doc:"write summary in FILE")
    in
    Cmd.v (Cmd.info ~doc "run")
      (Term.(const aux $ j $ pp_results $ dyn $ paths
             $ dir_file $ proof_dir $ defs $ task $ timeout $ memory
             $ meta $ provers $ csv $ summary $ no_color $ save))
end

module List_files = struct
  let main ?(abs=false) () : bool =
    catch_err @@ fun () ->
    let data_dir = Misc.data_dir() in
    let entries, _ = Bin_utils.list_entries data_dir in
    List.iter
      (fun (s,size) ->
         let s = if abs then s else Filename.basename s in
         Printf.printf "%s (%s)\n" s (Misc.human_size size))
      entries;
    ()

  (* sub-command to sample a directory *)
  let cmd =
    let open Cmdliner in
    let abs =
      Arg.(value & opt ~vopt:true bool false & info ["abs"] ~doc:"show absolute paths")
    in
    let doc = "list benchmark result files" in
    let aux abs () = main ~abs () in
    Cmd.v (Cmd.info ~doc "list-files")
      Term.(const aux $ abs $ const () )
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
    let aux check bad csv summary no_color debug file : bool =
      catch_err @@ fun () ->
      Misc.setup_logs debug;
      if no_color then CCFormat.set_color_default false;
      Show.main ~check ~bad ?csv ?summary file
    in
    let doc = "show benchmark results (see `list-files`)" in
    Cmd.v (Cmd.info ~doc "show")
      Term.(const aux $ check $ bad $ csv $ summary $ no_color $ debug $ file)
end

(** {2 plot results} *)

module Plot = struct
  let main file =
    Logs.debug (fun k->k "plot file %s" file);
    let file = Bin_utils.mk_file_full file in
    Db.with_db ~timeout:500 ~mode:`READONLY file
      (fun db ->
         let p = Cactus_plot.of_db db in
         Cactus_plot.show p;
         ())

  (* sub-command for showing results *)
  let cmd =
    let open Cmdliner in
    let file =
      Arg.(required & pos 0 (some string) None &
           info [] ~docv:"FILE" ~doc:"file to read")
    and debug =
      Logs_cli.level ()
    in
    let aux debug file =
      catch_err @@ fun () ->
      Misc.setup_logs debug;
      main file
    in
    let doc = "plot benchmark results" in
    Cmd.v (Cmd.info ~doc "plot")
      Term.(const aux $ debug $ file)
end

(** {2 Sample} *)
module Sample = struct
  let files_of_dir (p:string) : string list =
    Error.guard (Error.wrapf "expanding subdir of_dir %S" p) @@ fun () ->
    CCIO.File.walk_l p
    |> CCList.filter_map
      (fun (kind,f) -> match kind with
         | `File -> Some f
         | _ -> None)

  let run ~n dirs =
    catch_err @@ fun () ->
    let files = CCList.flat_map files_of_dir dirs |> Array.of_list in
    let len = Array.length files in
    if len < n then (
      Error.failf "not enough files (need %d, got %d)" n len
    );
    (* sample the list *)
    let sample_idx =
      CCRandom.sample_without_duplicates
        ~cmp:CCInt.compare n (CCRandom.int len)
      |> CCRandom.run ?st:None
    in
    let sample = CCList.map (Array.get files) sample_idx in
    (* print sample *)
    Misc.synchronized (fun () -> List.iter (Printf.printf "%s\n%!") sample);
    ()

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
    Cmd.v (Cmd.info ~doc "sample")
      Term.(const aux $ n $ dir)
end

(** {2 Show directories} *)

module Dir = struct
  type which = Config | State

  let which_conv = Cmdliner.Arg.(enum ["config", Config; "state", State])

  let run c =
    catch_err @@ fun () ->
    Format.printf "%s@."
      (match c with
       | Config -> Misc.config_dir()
       | State -> Misc.data_dir ());
    ()

  (* sub-command for showing results *)
  let cmd =
    let open Cmdliner in
    let which =
      Arg.(required & pos 0 (some which_conv) None & info ~doc:"directory to list (config|state)" [])
    in
    let doc = "show directories where benchpress stores its state (config|state)" in
    Cmd.v (Cmd.info ~doc "dir")
      Term.(const run $ which)

end

(** {2 Check config} *)

module Check_config = struct
  let run debug with_default f =
    catch_err @@ fun () ->
    Misc.setup_logs debug;
    let default_file = Misc.default_config () in
    let f =
      if f=[] then (
        if Sys.file_exists default_file then [default_file] else []
      ) else if with_default && Sys.file_exists default_file then Misc.default_config() :: f else f in
    let l = Stanza.parse_files f in
    Format.printf "@[<v>%a@]@." Stanza.pp_l l;
    let _defs = Definitions.of_stanza_l l in (* some checks are delayed *)
    ()

  let cmd =
    let open Cmdliner in
    let files =
      Arg.(value & pos_all string [] & info [] ~doc:"file(s) to check")
    and debug =
      Logs_cli.level ()
    and with_default =
      Arg.(value & opt bool false & info ["d"; "default"] ~doc:"combine with the default config file(s)")
    in
    let doc = "parse and print configuration file(s)" in
    let aux debug with_default files () = run debug with_default files in
    Cmd.v (Cmd.info ~doc "check-config")
      Term.(const aux $ debug $ with_default $ files $ const () )
end

(** {2 See prover(s)} *)

module Prover_show = struct
  let run defs names =
    catch_err @@ fun () ->
    let l = CCList.map (Definitions.find_prover' defs) names in
    Format.printf "@[<v>%a@]@." (Misc.pp_list Prover.pp) l;
    ()

  let cmd =
    let open Cmdliner in
    let doc = "show definitions of given prover(s)" in
    let names = Arg.(value & pos_all string [] & info []) in
    Cmd.v (Cmd.info ~doc "show-prover")
      Term.(const run $ Bin_utils.definitions_term $ names)
end

(** {2 List provers} *)

module Prover_list = struct
  let run defs =
    catch_err @@ fun () ->
    let l = Definitions.all_provers defs in
    Format.printf "@[<v>%a@]@." (Misc.pp_list @@ Fmt.map With_loc.view Prover.pp_name) l;
    ()

  let cmd =
    let open Cmdliner in
    let doc = "list prover(s) defined in config" in
    Cmd.v (Cmd.info ~doc "list-prover")
      Term.(const run $ Bin_utils.definitions_term)
end

(** {2 Show Task} *)

module Task_show = struct
  let run defs names =
    catch_err @@ fun () ->
    let l = CCList.map (Definitions.find_task' defs) names in
    Format.printf "@[<v>%a@]@." (Misc.pp_list Task.pp) l;
    ()

  let cmd =
    let open Cmdliner in
    let doc = "show definitions of given task(s)" in
    let names = Arg.(value & pos_all string [] & info []) in
    Cmd.v (Cmd.info ~doc "show-task")
      Term.(const run $ Bin_utils.definitions_term $ names)
end

(** {2 List Tasks} *)

module Task_list = struct
  let run defs =
    catch_err @@ fun () ->
    let l = Definitions.all_tasks defs in
    Format.printf "@[<v>%a@]@." (Misc.pp_list @@ Fmt.map With_loc.view Task.pp_name) l;
    ()

  let cmd =
    let open Cmdliner in
    let doc = "list task(s) defined in config" in
    Cmd.v (Cmd.info ~doc "list-task")
      Term.(const run $ Bin_utils.definitions_term)
end

(** {2 Convert results to Sql} *)

module Sql_convert = struct
  let run defs files =
    catch_err @@ fun () ->
    Sql_res.run defs files

  (* sub-command for showing results *)
  let cmd =
    let open Cmdliner in
    let files =
      Arg.(non_empty & pos_all string [] &
           info [] ~docv:"FILES" ~doc:"files to read")
    in
    let doc = "convert result(s) into sqlite files" in
    Cmd.v (Cmd.info ~doc "sql-convert")
      Term.(const run $ Bin_utils.definitions_term $ files)
end

(** {2 Main: Parse CLI} *)

let parse_opt () =
  let open Cmdliner in
  let default, info =
    let doc = "Tool to test logic solver and automatic theorem provers." in
    let man = [
      `S "DESCRIPTION";
      `P "$(b,benchpress) is a tool to run tests and compare different \
          results obtained with distinct tools or versions of the same tool";
      `S "COMMANDS";
      `S "OPTIONS"; (* TODO: explain config file *)
    ] in
    Term.(ret (const (fun () -> `Help (`Pager, None)) $ const ())),
    Cmd.info ~version:"dev" ~man ~doc "benchpress"
  in
  let cmds = [
    Dir.cmd;
    Run.cmd;
    Sample.cmd;
    List_files.cmd;
    Show.cmd;
    Check_config.cmd;
    Prover_show.cmd;
    Prover_list.cmd;
    Sql_convert.cmd;
    Task_list.cmd;
    Task_show.cmd;
    Plot.cmd;
  ] in
  Cmd.eval_value (Cmd.group info ~default cmds)

let () =
  CCFormat.set_color_default true;
  if Sys.getenv_opt "PROFILE"=Some "1" then Profile.enable();
  match parse_opt () with
  | Error (`Parse | `Term | `Exn) -> exit 2
  | Ok (`Ok true | `Version | `Help) -> ()
  | Ok `Ok false -> exit 1

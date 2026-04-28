(* This file is free software. See file "license" for more details. *)

(* run tests, or compare results *)

open Common
module T = Test
module Db = Sqlite3_utils

let catch_err f =
  try
    f ();
    true
  with Error.E e ->
    Format.eprintf "%a@." Error.pp e;
    false

(** {2 Run} *)
module Run = struct
  (* Custom CPU spec parser - kept separate due to complex logic *)
  let cpus_term =
    let open Cmdliner in
    let doc =
      "Limit the specific CPUs or cores to use. When provided, the\n\
      \      [-j] flag is ignored, and each prover gets allocated its own CPU \
       core from\n\
      \      this list. A comma-separated list or hyphen-separated ranges are \
       allowed."
    in
    let parser s =
      match String.split_on_char '-' s with
      | [] -> assert false (* [split_on_char] invariant *)
      | [ n ] -> Result.map (fun x -> x, x) Arg.(conv_parser int n)
      | [ n; m ] ->
        Result.bind Arg.(conv_parser int n) @@ fun n ->
        Result.bind Arg.(conv_parser int m) @@ fun m ->
        if m < n then
          Error (`Msg (Format.asprintf "invalid range: %d-%d" n m))
        else
          Ok (n, m)
      | _ -> Error (`Msg (Format.asprintf "invalid cpuset: %s" s))
    in
    let printer ppf (n, m) =
      if n = m then
        Format.pp_print_int ppf n
      else
        Format.fprintf ppf "%d-%d" n m
    in
    let cpuspec = Arg.conv ~docv:"MASK" (parser, printer) in
    let parse xs =
      let cpus =
        CCList.flat_map
          (fun (n, m) -> List.init (m + 1 - n) (fun i -> i + n))
          xs
        |> List.sort_uniq Int.compare
      in
      match cpus with
      | [] -> None
      | _ -> Some cpus
    in
    Term.(
      const parse $ Arg.(value & opt (list cpuspec) [] & info [ "cpus" ] ~doc))

  (* Parameters using ppx_subliner *)
  type params = {
    j: int; [@default 1]  (** level of parallelism *)
    progress: bool;  (** print progress bar *)
    paths: string list; [@pos_all] [@docv "PATH"]
        (** target paths (or directories containing tests) *)
    dir_files: string list; [@opt_all] [@names [ "F" ]] [@default []]
        (** file containing a list of files *)
    proof_dir: string option;  (** store proofs in given directory *)
    task: string option;  (** task to run *)
    timeout: int option; [@names [ "t"; "timeout" ]]  (** timeout (in s) *)
    memory: int option; [@names [ "m"; "memory" ]]  (** memory (in MB) *)
    meta: string; [@default ""]  (** additional metadata to save *)
    provers: string list; [@opt_all] [@names [ "p"; "provers" ]] [@default []]
        (** select provers *)
    csv: string option;  (** CSV output file *)
    summary: string option;  (** write summary in FILE *)
    color: bool;  (** enable colored output *)
    output: string option; [@names [ "o"; "output" ]]
        (** output database file *)
    save: bool; [@default true]  (** save results on disk *)
    wal_mode: bool; [@names [ "wal" ]]  (** turn on the journal WAL mode *)
    desktop_notification: bool;
        [@default true] [@names [ "desktop-notification"; "dn" ]]
        (** send a desktop notification when the benchmarking is done (true by
            default) *)
    no_failure: bool; [@names [ "no-failure"; "nf" ]]
        (** don't fail if some provers give incorrect answers (contradictory to
            what was expected) *)
    update: bool; [@names [ "update"; "u" ]]
        (** if the output file already exists, overwrite it with the new one. *)
  }
  [@@deriving subliner]

  let run (p : params) cpus (log_lvl, defs) =
    Misc.setup_logs log_lvl;
    catch_err @@ fun () ->
    if p.color then CCFormat.set_color_default true;
    let dyn =
      if p.progress then
        Some true
      else
        None
    in
    Run_main.main ~pp_results:p.progress ?dyn ~j:p.j ?cpus ?timeout:p.timeout
      ?memory:p.memory ?csv:p.csv ~provers:p.provers ~meta:p.meta ?task:p.task
      ?summary:p.summary ~dir_files:p.dir_files ?proof_dir:p.proof_dir
      ?output:p.output ~save:p.save ~wal_mode:p.wal_mode
      ~desktop_notification:p.desktop_notification ~no_failure:p.no_failure
      ~update:p.update defs p.paths ()

  let cmd =
    let doc =
      "run a task, such as running solvers on directories of problem files"
    in
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info ~doc "run")
      Cmdliner.Term.(
        const run $ params_cmdliner_term () $ cpus_term
        $ Bin_utils.definitions_term)
end

module Slurm = struct
  (* sub-command for running tests with slurm *)
  type params = {
    j: int; [@default 1]
        (** number of parallel threads each worker will launch on the node on
            which it's running. *)
    progress: bool;  (** print progress bar *)
    paths: string list; [@pos_all] [@docv "PATH"]
        (** target paths (or directories containing tests) *)
    dir_files: string list; [@opt_all] [@names [ "F" ]] [@default []]
        (** file containing a list of files *)
    proof_dir: string option;  (** store proofs in given directory *)
    task: string option;  (** task to run *)
    timeout: int option; [@names [ "t"; "timeout" ]]  (** timeout (in s) *)
    memory: int option; [@names [ "m"; "memory" ]]  (** memory (in MB) *)
    meta: string; [@default ""]  (** additional metadata to save *)
    provers: string list; [@opt_all] [@names [ "p"; "provers" ]] [@default []]
        (** select provers *)
    csv: string option;  (** CSV output file *)
    summary: string option;  (** write summary in FILE *)
    color: bool;  (** enable colored output *)
    output: string option; [@names [ "o"; "output" ]]
        (** output database file *)
    save: bool; [@default true]  (** save results on disk *)
    wal_mode: bool; [@names [ "wal" ]]  (** turn on the journal WAL mode *)
    desktop_notification: bool;
        [@default true] [@names [ "desktop-notification"; "dn" ]]
        (** send a desktop notification when the benchmarking is done (true by
            default) *)
    no_failure: bool; [@names [ "no-failure"; "nf" ]]
        (** don't fail if some provers give incorrect answers (contradictory to
            what was expected) *)
    update: bool; [@names [ "update"; "u" ]]
        (** if the output file already exists, overwrite it with the new one. *)
    partition: string option;
        (** partition to which the allocated nodes should belong *)
    nodes: int option; [@names [ "n"; "nodes" ]]
        (** the maximum number of nodes to be used *)
    addr: string option; [@names [ "a"; "addr" ]]
        (** IP address of the server on the control node. Needs to be reachable
            by the workers which will run on the allocated calculation nodes. *)
    port: int option;
        (** port of the server on the control node. Default is 0 to let the OS
            choose a port. *)
    ntasks: int option;
        (** The number of tasks to give the workers at a time. *)
  }
  [@@deriving subliner]

  let run (p : params) (log_lvl, defs) =
    Misc.setup_logs log_lvl;
    let@ () = catch_err in
    if p.color then CCFormat.set_color_default true;
    let dyn =
      if p.progress then
        Some true
      else
        None
    in
    let addr =
      match p.addr with
      | None -> None
      | Some s -> Some (Unix.inet_addr_of_string s)
    in
    Run_main.main ~sbatch:true ~pp_results:p.progress ?dyn ~j:p.j
      ?timeout:p.timeout ?memory:p.memory ?csv:p.csv ~provers:p.provers
      ~meta:p.meta ?task:p.task ?summary:p.summary ~dir_files:p.dir_files
      ?proof_dir:p.proof_dir ?output:p.output ~wal_mode:p.wal_mode
      ~desktop_notification:p.desktop_notification ~no_failure:p.no_failure
      ~update:p.update ~save:p.save ?partition:p.partition ?nodes:p.nodes ?addr
      ?port:p.port ?ntasks:p.ntasks defs p.paths ()

  let cmd =
    let doc =
      "run benchpress using the computing power of a cluster that works with \
       slurm"
    in
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info ~doc "slurm")
      Cmdliner.Term.(
        const run $ params_cmdliner_term () $ Bin_utils.definitions_term)
end

module List_files = struct
  let main ?(abs = false) () : bool =
    let@ () = catch_err in
    let data_dir = Misc.data_dir () in
    let entries, _ = Bin_utils.list_entries data_dir in
    List.iter
      (fun (s, size) ->
        let s =
          if abs then
            s
          else
            Filename.basename s
        in
        Printf.printf "%s (%s)\n" s (Misc.human_size size))
      entries;
    ()

  type params = { abs: bool [@default false]  (** show absolute paths *) }
  [@@deriving subliner]

  let cmd =
    let doc = "list benchmark result files" in
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info ~doc "list-files")
      Cmdliner.Term.(
        const (fun p -> main ~abs:p.abs ()) $ params_cmdliner_term ())
end

module Show = struct
  type params = {
    csv: bool;  (** CSV output to stdout *)
    csv_file: string option;  (** CSV output file *)
    jsonl: bool;  (** JSONL output to stdout *)
    jsonl_file: string option;  (** JSONL output file *)
    file: string option; [@pos 0] [@docv "FILE"]
        (** file to read (default: latest) *)
    color: bool;  (** enable colored output *)
    check: bool;  (** check results *)
    bad: bool;  (** list bad results *)
    summary: string option;  (** write summary in FILE *)
    details: bool;  (** show more details *)
  }
  [@@deriving subliner]

  let run (p : params) debug =
    let@ () = catch_err in
    Misc.setup_logs debug;
    if p.color then CCFormat.set_color_default true;
    let file =
      match p.file with
      | Some f -> f
      | None ->
        (* Get the latest file from the data directory *)
        let data_dir = Filename.concat (Xdg.data_dir ()) !Xdg.name_of_project in
        let files, _ = Bin_utils.list_entries data_dir ~limit:1 in
        (match files with
        | (path, _) :: _ -> Filename.basename path
        | [] -> Error.failf "no result files found in %s" data_dir)
    in
    Show.main ~check:p.check ~bad:p.bad ~details:p.details ~csv:p.csv
      ?csv_file:p.csv_file ~jsonl:p.jsonl ?jsonl_file:p.jsonl_file
      ?summary:p.summary file

  let cmd =
    let doc = "show benchmark results (see `list-files`)" in
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info ~doc "show")
      Cmdliner.Term.(const run $ params_cmdliner_term () $ Logs_cli.level ())
end

(** {2 Sample} *)
module Sample = struct
  let files_of_dir (p : string) : string list =
    let@ () = Error.guard (Error.wrapf "expanding subdir of_dir %S" p) in
    CCIO.File.walk_l p
    |> CCList.filter_map (fun (kind, f) ->
           match kind with
           | `File -> Some f
           | _ -> None)

  let run ~n dirs =
    let@ () = catch_err in
    let files = CCList.flat_map files_of_dir dirs |> Array.of_list in
    let len = Array.length files in
    if len < n then Error.failf "not enough files (need %d, got %d)" n len;
    (* sample the list *)
    let sample_idx =
      CCRandom.sample_without_duplicates ~cmp:CCInt.compare n (CCRandom.int len)
      |> CCRandom.run ?st:None
    in
    let sample = CCList.map (Array.get files) sample_idx in
    (* print sample *)
    Misc.synchronized_sync (fun () -> List.iter (Printf.printf "%s\n%!") sample);
    ()

  type params = {
    dirs: string list; [@pos_all] [@docv "DIR"]
        (** target directories (containing tests) *)
    n: int; [@default 1] [@docv "N"]  (** number of files to sample *)
  }
  [@@deriving subliner]

  let cmd =
    let doc = "sample N files in the given directories" in
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info ~doc "sample")
      Cmdliner.Term.(
        const (fun p -> run ~n:p.n p.dirs) $ params_cmdliner_term ())
end

(** {2 Show directories} *)

module Dir = struct
  type which = Config | State [@@deriving subliner_enum]

  let run c =
    let@ () = catch_err in
    Format.printf "%s@."
      (match c with
      | Config -> Misc.config_dir ()
      | State -> Misc.data_dir ());
    ()

  let cmd =
    let open Cmdliner in
    let which =
      Arg.(required & pos 0 (some (which_cmdliner_conv ())) None & info [])
    in
    let doc =
      "show directories where benchpress stores its state (config|state)"
    in
    Cmd.v (Cmd.info ~doc "dir") Term.(const run $ which)
end

(** {2 Check config} *)

module Check_config = struct
  type params = {
    files: string list; [@pos_all] [@default []]  (** file(s) to check *)
    with_default: bool; [@default false] [@names [ "d"; "default" ]]
        (** combine with the default config file(s) *)
  }
  [@@deriving subliner]

  let run (p : params) debug =
    let@ () = catch_err in
    Misc.setup_logs debug;
    let default_file = Misc.default_config () in
    let f =
      if p.files = [] then
        if Sys.file_exists default_file then
          [ default_file ]
        else
          []
      else if p.with_default && Sys.file_exists default_file then
        Misc.default_config () :: p.files
      else
        p.files
    in
    let l = Stanza.parse_files f in
    Format.printf "@[<v>%a@]@." Stanza.pp_l l;
    let _defs = Definitions.of_stanza_l l in
    (* some checks are delayed *)
    ()

  let cmd =
    let doc = "parse and print configuration file(s)" in
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info ~doc "check-config")
      Cmdliner.Term.(const run $ params_cmdliner_term () $ Logs_cli.level ())
end

(** {2 See prover(s)} *)

module Prover_show = struct
  type params = { names: string list [@pos_all] [@default []] }
  [@@deriving subliner]

  let run (p : params) (log_lvl, defs) =
    Misc.setup_logs log_lvl;
    let@ () = catch_err in
    let l = CCList.map (Definitions.find_prover' defs) p.names in
    Format.printf "@[<v>%a@]@." (Misc.pp_list Prover.pp) l;
    ()

  let cmd =
    let doc = "show definitions of given prover(s)" in
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info ~doc "show-prover")
      Cmdliner.Term.(
        const run $ params_cmdliner_term () $ Bin_utils.definitions_term)
end

(** {2 List provers} *)

module Prover_list = struct
  let run (log_lvl, defs) =
    Misc.setup_logs log_lvl;
    let@ () = catch_err in
    let l = Definitions.all_provers defs in
    Format.printf "@[<v>%a@]@."
      (Misc.pp_list @@ Fmt.map With_loc.view Prover.pp_name)
      l;
    ()

  let cmd =
    let doc = "list prover(s) defined in config" in
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info ~doc "list-prover")
      Cmdliner.Term.(const run $ Bin_utils.definitions_term)
end

(** {2 Show Task} *)

module Task_show = struct
  type params = { names: string list [@pos_all] [@default []] }
  [@@deriving subliner]

  let run (p : params) (log_lvl, defs) =
    Misc.setup_logs log_lvl;
    let@ () = catch_err in
    let l = CCList.map (Definitions.find_task' defs) p.names in
    Format.printf "@[<v>%a@]@." (Misc.pp_list Task.pp) l;
    ()

  let cmd =
    let doc = "show definitions of given task(s)" in
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info ~doc "show-task")
      Cmdliner.Term.(
        const run $ params_cmdliner_term () $ Bin_utils.definitions_term)
end

(** {2 List Tasks} *)

module Task_list = struct
  let run (log_lvl, defs) =
    Misc.setup_logs log_lvl;
    let@ () = catch_err in
    let l = Definitions.all_tasks defs in
    Format.printf "@[<v>%a@]@."
      (Misc.pp_list @@ Fmt.map With_loc.view Task.pp_name)
      l;
    ()

  let cmd =
    let doc = "list task(s) defined in config" in
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info ~doc "list-task")
      Cmdliner.Term.(const run $ Bin_utils.definitions_term)
end

(** {2 Convert results to Sql} *)

module Sql_convert = struct
  type params = {
    files: string list; [@pos_all] [@non_empty] [@docv "FILES"]
        (** files to read *)
  }
  [@@deriving subliner]

  let run (p : params) defs =
    let@ () = catch_err in
    Sql_res.run defs p.files

  let cmd =
    let doc = "convert result(s) into sqlite files" in
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info ~doc "sql-convert")
      Cmdliner.Term.(
        const run $ params_cmdliner_term () $ Bin_utils.definitions_term)
end

(** {2 Main: Parse CLI} *)

let parse_opt () =
  let open Cmdliner in
  let default, info =
    let doc = "Tool to test logic solver and automatic theorem provers." in
    let man =
      [
        `S "DESCRIPTION";
        `P
          "$(b,benchpress) is a tool to run tests and compare different \
           results obtained with distinct tools or versions of the same tool";
        `S "COMMANDS";
        `S "OPTIONS";
        (* TODO: explain config file *)
      ]
    in
    ( Term.(ret (const (fun () -> `Help (`Pager, None)) $ const ())),
      Cmd.info ~version:"dev" ~man ~doc "benchpress" )
  in
  let cmds =
    [
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
      Slurm.cmd;
    ]
  in
  Cmd.eval_value (Cmd.group info ~default cmds)

let () =
  let@ () = Trace_tef.with_setup () in
  let@ env = Eio_posix.run in
  Trace_eio.setup ();
  let proc_mgr = Eio.Stdenv.process_mgr env in
  Run_proc.with_proc_mgr proc_mgr @@ fun () ->
  match parse_opt () with
  | Error (`Parse | `Term | `Exn) -> exit 2
  | Ok (`Ok true | `Version | `Help) -> ()
  | Ok (`Ok false) -> exit 1

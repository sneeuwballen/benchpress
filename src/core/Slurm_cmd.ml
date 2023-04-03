(** [sbatch ?options script] creates a "sbatch" command that submits the script located in the path [script] with the command line options [?options]. *)
let sbatch ?(options = []) ?(wrap = false) target =
  Format.sprintf "%s %s"
    (Misc.mk_shell_cmd ~options "sbatch")
    (if wrap then
      Format.sprintf "--wrap=\"%s\"" target
    else
      target)

(** [srun ?options cmd] creates an "srun" command that executes the command [cmd] with the command line options [?options]. *)
let srun ?(options = []) cmd =
  Format.sprintf "%s %s" (Misc.mk_shell_cmd ~options "srun") cmd

(** [grep_job_id sbatch_cmd] Given a "sbatch" command, generates a command
    that extracts from the output of the "sbatch" command the ID of the job
    that was submitted. *)
let grep_job_id sbatch_cmd =
  Format.sprintf "%s | grep -oP \"^Submitted batch job \\K[0-9]+$\"" sbatch_cmd

(** [scancel job_id] creates the command that runs "scancel" on [job_id]. *)
let scancel job_id = Format.sprintf "scancel %d" job_id

(** [mk_sbatch_cmds limits proof_dir j addr port partition config_file nodes]
    creates a list of [n] sbatch commands parametrized with the provided
    options. *)
let mk_sbatch_cmds limits proof_dir j addr port partition config_file n =
  let acc_aux opt v cond f acc =
    if cond v then
      (opt, f v) :: acc
    else
      acc
  in
  let aux_acc_limits (limits : Limit.All.t) acc =
    acc_aux "-t" limits.time Option.is_some
      (Option.map (fun t -> string_of_int Limit.(Time.as_int Time.Seconds t)))
    @@ acc_aux "-m" limits.memory Option.is_some
         (Option.map (fun m ->
              string_of_int Limit.(Memory.as_int Memory.Megabytes m)))
         acc
  in
  let worker_cmd =
    let worker_exec =
      Format.sprintf "%s/benchpress_worker.exe" (Sys.getcwd ())
    in
    let worker_cmd_opts =
      aux_acc_limits limits
      @@ acc_aux "--proof-dir" proof_dir Option.is_some Fun.id
      @@ acc_aux "-j" j
           (function
             | i when i > 0 -> true
             | _ -> false)
           (fun i -> Some (string_of_int i))
      @@ [
           "-a", Some (Unix.string_of_inet_addr addr);
           "-p", Some port;
           "-c", Some config_file;
         ]
    in
    fun id ->
      let options = ("--id", Some (string_of_int id)) :: worker_cmd_opts in
      Misc.mk_shell_cmd ~options worker_exec ^ " >> tmp.txt"
  in
  let options =
    acc_aux "--partition" partition Option.is_some Fun.id
    @@ [ "--nodes", Some "1"; "--exclusive", None; "--mem", Some "0" ]
  in
  let wrap = true in
  List.init n (fun id ->
      grep_job_id (sbatch (worker_cmd (id + 1)) ~options ~wrap))

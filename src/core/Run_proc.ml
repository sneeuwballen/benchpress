module Log = (val Logs.src_log (Logs.Src.create "run-proc"))

let int_of_process_status = function
  | Unix.WEXITED i | Unix.WSIGNALED i | Unix.WSTOPPED i -> i

(* There is no version of [create_process] that opens a shell, so we roll our
   own version of [open_process] . *)
let sh cmd =
  if Sys.unix || Sys.cygwin then
    Unix.create_process "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
  else if Sys.win32 then (
    let shell =
      try Sys.getenv "COMSPEC"
      with Not_found -> raise Unix.(Unix_error (ENOEXEC, "sh", cmd))
    in
    Unix.create_process shell [| shell; "/c"; cmd |]
  ) else
    Format.kasprintf failwith "Unsupported OS type: %s" Sys.os_type

(* Available as [Filename.null] on OCaml >= 4.10 *)
let null () =
  if Sys.unix || Sys.cygwin then
    "/dev/null"
  else if Sys.win32 then
    "NUL"
  else
    (* Nobody is running benchpress with js_of_ocaml… right? *)
    Format.kasprintf failwith "Unsupported OS type: %s" Sys.os_type

let run cmd : Run_proc_result.t =
  (* create temporary files for stdout and stderr *)
  try
    Misc.with_ram_file "stdout" "" @@ fun stdout_f ->
    Misc.with_ram_file "stderr" "" @@ fun stderr_f ->
    let stdout_fd = Unix.openfile stdout_f [ O_WRONLY; O_KEEPEXEC ] 0o640 in
    let stderr_fd = Unix.openfile stderr_f [ O_WRONLY; O_KEEPEXEC ] 0o640 in
    let stdin_fd = Unix.openfile (null ()) [ O_RDONLY; O_KEEPEXEC ] 0o000 in
    (* call process and block *)
    let errcode, rtime =
      let start = Ptime_clock.now () in
      let pid = sh cmd stdin_fd stdout_fd stderr_fd in
      let _, status = Unix.waitpid [] pid in
      (* Compute time used by the command *)
      let rtime =
        Ptime.diff (Ptime_clock.now ()) start |> Ptime.Span.to_float_s
      in
      int_of_process_status status, rtime
    in
    let stdout = CCIO.with_in stdout_f @@ CCIO.read_all in
    let stderr = CCIO.with_in stderr_f @@ CCIO.read_all in
    Unix.close stdout_fd;
    Unix.close stderr_fd;
    Unix.close stdin_fd;
    Log.debug (fun k ->
        k "(@[run.done@ :errcode %d@ :cmd %a@ :stdout %s@ :stdout_f %s@]"
          errcode Misc.Pp.pp_str cmd stdout stdout_f);
    let utime = 0. in
    let stime = 0. in
    Log.debug (fun k -> k "stdout:\n%s\nstderr:\n%s" stdout stderr);
    { Run_proc_result.stdout; stderr; errcode; rtime; utime; stime }
  with e ->
    {
      stdout = "";
      stderr = "benchpress error: " ^ Printexc.to_string e;
      errcode = 1;
      rtime = 0.;
      utime = 0.;
      stime = 0.;
    }

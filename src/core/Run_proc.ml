module Log = (val Logs.src_log (Logs.Src.create "run-proc"))

let int_of_process_status = function
  | Unix.WEXITED i | Unix.WSIGNALED i | Unix.WSTOPPED i -> i

let run cmd : Run_proc_result.t =
  let start = Ptime_clock.now () in
  (* call process and block *)
  let p =
    try
      let oc, ic, errc = Unix.open_process_full cmd (Unix.environment ()) in
      close_out ic;
      (* read out and err *)
      let err = ref "" in
      let t_err = Thread.create (fun e -> err := CCIO.read_all e) errc in
      let out = CCIO.read_all oc in
      Thread.join t_err;
      let status = Unix.close_process_full (oc, ic, errc) in
      object
        method stdout = out
        method stderr = !err
        method errcode = int_of_process_status status
        method status = status
      end
    with e ->
      object
        method stdout = ""
        method stderr = "process died: " ^ Printexc.to_string e
        method errcode = 1
        method status = Unix.WEXITED 1
      end
  in
  let errcode = p#errcode in
  Log.debug (fun k ->
      k "(@[run.done@ :errcode %d@ :cmd %a@]" errcode Misc.Pp.pp_str cmd);
  (* Compute time used by the command *)
  let rtime = Ptime.diff (Ptime_clock.now ()) start |> Ptime.Span.to_float_s in
  let utime = 0. in
  let stime = 0. in
  let stdout = p#stdout in
  let stderr = p#stderr in
  Log.debug (fun k -> k "stdout:\n%s\nstderr:\n%s" stdout stderr);
  { Run_proc_result.stdout; stderr; errcode; rtime; utime; stime }

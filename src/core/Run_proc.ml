module Log = (val Logs.src_log (Logs.Src.create "run-proc"))

(** Fiber-local key holding the Eio process manager. Must be bound (via
    [with_proc_mgr]) before calling [run]. *)
let k_proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t Eio.Fiber.key =
  Eio.Fiber.create_key ()

let with_proc_mgr mgr f = Eio.Fiber.with_binding k_proc_mgr mgr f

let run cmd : Run_proc_result.t =
  let start = Ptime_clock.now () in

  Unix.putenv "TRACE" "";
  Unix.putenv "LOG" "";

  let proc_mgr =
    match Eio.Fiber.get k_proc_mgr with
    | Some m -> m
    | None -> Error.fail "Run_proc.run: no process manager in fiber context"
  in

  (* Capture stdout and stderr via pipes, run concurrently with Eio fibers *)
  let stdout, stderr, errcode =
    try
      Eio.Switch.run @@ fun sw ->
      let stdout_r, stdout_w = Eio_unix.pipe sw in
      let stderr_r, stderr_w = Eio_unix.pipe sw in
      let child =
        Eio.Process.spawn ~sw proc_mgr
          ~stdout:(stdout_w :> Eio.Flow.sink_ty Eio.Resource.t)
          ~stderr:(stderr_w :> Eio.Flow.sink_ty Eio.Resource.t)
          [ "/bin/sh"; "-c"; cmd ]
      in
      (* Close write-ends in parent so reads terminate at child exit *)
      Eio.Resource.close stdout_w;
      Eio.Resource.close stderr_w;
      (* Read both streams concurrently *)
      let out = ref "" and err = ref "" in
      Eio.Fiber.both
        (fun () -> out := Eio.Flow.read_all stdout_r)
        (fun () -> err := Eio.Flow.read_all stderr_r);
      let status = Eio.Process.await child in
      let errcode =
        match status with
        | `Exited n | `Signaled n -> n
      in
      !out, !err, errcode
    with e -> "", "process died: " ^ Printexc.to_string e, 1
  in

  Log.debug (fun k ->
      k "(@[run.done@ :errcode %d@ :cmd %a@]" errcode Misc.Pp.pp_str cmd);
  let rtime = Ptime.diff (Ptime_clock.now ()) start |> Ptime.Span.to_float_s in
  let utime = 0. in
  let stime = 0. in
  Log.debug (fun k -> k "stdout:\n%s\nstderr:\n%s" stdout stderr);
  { Run_proc_result.stdout; stderr; errcode; rtime; utime; stime }

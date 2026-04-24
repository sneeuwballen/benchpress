module Log = (val Logs.src_log (Logs.Src.create "run-proc"))

(** Fiber-local key holding the Eio process manager. Must be bound (via
    [with_proc_mgr]) before calling [run]. *)
let k_proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t Eio.Fiber.key =
  Eio.Fiber.create_key ()

let with_proc_mgr mgr f = Eio.Fiber.with_binding k_proc_mgr mgr f

(** Vars to strip from child env to avoid inheriting benchpress's tracing/logging setup. *)
let strip_from_child_env = [| "TRACE"; "LOG" |]

(** Build child environment: current env minus [strip_from_child_env]. *)
let child_env () : string array =
  Unix.environment ()
  |> Array.to_seq
  |> Seq.filter (fun entry ->
         not
           (Array.exists
              (fun key ->
                let pfx = key ^ "=" in
                String.length entry >= String.length pfx
                && String.sub entry 0 (String.length pfx) = pfx)
              strip_from_child_env))
  |> Array.of_seq

let run cmd : Run_proc_result.t =
  let start = Ptime_clock.now () in

  let proc_mgr =
    match Eio.Fiber.get k_proc_mgr with
    | Some m -> m
    | None -> Error.fail "Run_proc.run: no process manager in fiber context"
  in

  let env = child_env () in

  (* Capture stdout and stderr via pipes.

     We race two alternatives with Eio.Fiber.any:
       1. Both drain fibers complete naturally (pipe write-ends closed on child exit).
       2. A watcher fiber observes the direct child exiting and cancels the drains.

     The watcher yields once before awaiting the child so the drain fibers get
     scheduled first and can drain whatever is already in the pipe buffer.  This
     ensures we get full output in the common case (no background grandchildren)
     while also not hanging when grandchildren keep the write-ends open. *)
  let stdout, stderr, errcode =
    try
      Eio.Switch.run @@ fun sw ->
      let stdin_r, stdin_w = Eio_unix.pipe sw in
      let stdout_r, stdout_w = Eio_unix.pipe sw in
      let stderr_r, stderr_w = Eio_unix.pipe sw in
      (* Close stdin write-end immediately so child sees EOF on stdin. *)
      Eio.Resource.close stdin_w;
      let child =
        Eio.Process.spawn ~sw proc_mgr ~env
          ~stdin:(stdin_r :> Eio.Flow.source_ty Eio.Resource.t)
          ~stdout:(stdout_w :> Eio.Flow.sink_ty Eio.Resource.t)
          ~stderr:(stderr_w :> Eio.Flow.sink_ty Eio.Resource.t)
          [ "/bin/sh"; "-c"; cmd ]
      in
      Eio.Resource.close stdin_r;
      Eio.Resource.close stdout_w;
      Eio.Resource.close stderr_w;
      let buf_out = Buffer.create 256 and buf_err = Buffer.create 64 in
      Eio.Fiber.any
        [
          (fun () ->
            Eio.Fiber.both
              (fun () ->
                Eio.Flow.copy stdout_r (Eio.Flow.buffer_sink buf_out))
              (fun () ->
                Eio.Flow.copy stderr_r (Eio.Flow.buffer_sink buf_err)));
          (fun () ->
            (* Yield first so drain fibers are scheduled and can drain buffered
               data before we decide to cancel them. *)
            Eio.Fiber.yield ();
            ignore (Eio.Process.await child));
        ];
      let status = Eio.Process.await child in
      let errcode =
        match status with
        | `Exited n | `Signaled n -> n
      in
      Buffer.contents buf_out, Buffer.contents buf_err, errcode
    with e -> "", "process died: " ^ Printexc.to_string e, 1
  in

  Log.debug (fun k ->
      k "(@[run.done@ :errcode %d@ :cmd %a@]" errcode Misc.Pp.pp_str cmd);
  let rtime = Ptime.diff (Ptime_clock.now ()) start |> Ptime.Span.to_float_s in
  let utime = 0. in
  let stime = 0. in
  Log.debug (fun k -> k "stdout:\n%s\nstderr:\n%s" stdout stderr);
  { Run_proc_result.stdout; stderr; errcode; rtime; utime; stime }

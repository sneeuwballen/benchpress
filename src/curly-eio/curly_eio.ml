(** Eio-based runner for the [curly] HTTP client library.

    Spawns [curl] as a subprocess via Eio (instead of [Unix]), reads the
    response from stdout, and parses it the same way [Curly] does.

    Usage:
    {[
      Curly_eio.with_proc_mgr my_mgr @@ fun () ->
      let res = Curly_eio.run (Curly_eio.Request.make ~url ~meth:`GET ()) in
      ...
    ]} *)

(** {2 Types (same shape as [curly])} *)

module Meth = struct
  type t =
    [ `GET
    | `POST
    | `HEAD
    | `PUT
    | `DELETE
    | `OPTIONS
    | `TRACE
    | `CONNECT
    | `PATCH
    | `Other of string ]

  let to_string = function
    | `GET -> "GET"
    | `POST -> "POST"
    | `HEAD -> "HEAD"
    | `PUT -> "PUT"
    | `DELETE -> "DELETE"
    | `OPTIONS -> "OPTIONS"
    | `TRACE -> "TRACE"
    | `CONNECT -> "CONNECT"
    | `PATCH -> "PATCH"
    | `Other s -> s

  let pp fmt t = Format.fprintf fmt "%s" (to_string t)
end

module Header = struct
  type t = (string * string) list

  let empty = []

  let to_cmd t =
    List.concat
      (List.map (fun (k, v) -> [ "-H"; Printf.sprintf "%s: %s" k v ]) t)

  let pp fmt t =
    Format.pp_print_list ~pp_sep:Format.pp_print_newline
      (fun fmt (k, v) -> Format.fprintf fmt "%s: %s\n" k v)
      fmt t
end

module Response = struct
  type t = { code: int; headers: Header.t; body: string }

  let pp fmt t =
    Format.fprintf fmt "{code=%d;@ headers=%a;@ body=\"%s\"}" t.code Header.pp
      t.headers t.body
end

module Request = struct
  type t = { meth: Meth.t; url: string; headers: Header.t; body: string }

  let make ?(headers = Header.empty) ?(body = "") ~url ~meth () =
    { meth; url; headers; body }

  let has_body t = String.length t.body > 0

  let to_cmd_args t =
    List.concat
      [
        [ "-X"; Meth.to_string t.meth ];
        Header.to_cmd t.headers;
        [ t.url ];
        (if has_body t then
           [ "--data-binary"; "@-" ]
         else
           []);
      ]

  let pp fmt t =
    Format.fprintf fmt
      "{@ meth=%a;@ url=\"%s\";@ headers=\"%a\";@ body=\"%s\"@ }" Meth.pp t.meth
      t.url Header.pp t.headers t.body
end

module Process_result = struct
  type t = { status: Unix.process_status; stderr: string; stdout: string }

  let pp_process_status fmt (exit : Unix.process_status) =
    match exit with
    | WEXITED n -> Format.fprintf fmt "Exit code %d" n
    | WSIGNALED n -> Format.fprintf fmt "Signal %d" n
    | WSTOPPED n -> Format.fprintf fmt "Stopped %d" n

  let pp fmt t =
    Format.fprintf fmt "{status=%a;@ stderr=\"%s\";@ stdout=\"%s\"}"
      pp_process_status t.status t.stderr t.stdout
end

module Error = struct
  type t =
    | Invalid_request of string
    | Bad_exit of Process_result.t
    | Failed_to_read_response of exn * Process_result.t
    | Exn of exn

  let pp fmt = function
    | Bad_exit p ->
      Format.fprintf fmt "Non 0 exit code %a@.%a"
        Process_result.pp_process_status p.status Process_result.pp p
    | Failed_to_read_response (e, _) ->
      Format.fprintf fmt "Couldn't read response:@ %s" (Printexc.to_string e)
    | Invalid_request r -> Format.fprintf fmt "Invalid request: %s" r
    | Exn e -> Format.fprintf fmt "Exception: %s" (Printexc.to_string e)
end

(** {2 HTTP response parser} *)

let response_of_stdout s =
  let lexbuf = Lexing.from_string s in
  try
    let r = Http.response { Http.code = 0; headers = []; body = "" } lexbuf in
    Ok
      ({ Response.code = r.code; headers = r.headers; body = r.body }
        : Response.t)
  with e -> Error e

(** {2 Process manager (fiber-local)} *)

let k_proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t Eio.Fiber.key =
  Eio.Fiber.create_key ()

let with_proc_mgr mgr f = Eio.Fiber.with_binding k_proc_mgr mgr f

(** {2 Helpers} *)

let is_prefix_ci s ~prefix =
  let s_len = String.length s in
  let prefix_len = String.length prefix in
  s_len >= prefix_len
  && String.equal
       (String.lowercase_ascii (String.sub s 0 prefix_len))
       (String.lowercase_ascii prefix)

let curl_env () =
  let kept_variables = [ "PATH"; "SYSTEMROOT" ] in
  Array.of_list
    (List.filter
       (fun entry ->
         List.exists
           (fun var -> is_prefix_ci ~prefix:(var ^ "=") entry)
           kept_variables)
       (Array.to_list (Unix.environment ())))

(** {2 Core: run curl via Eio} *)

let run_prog prog args stdin_str =
  let proc_mgr =
    match Eio.Fiber.get k_proc_mgr with
    | Some m -> m
    | None ->
      failwith
        "Curly_eio.run: no process manager in fiber context. Use \
         Curly_eio.with_proc_mgr."
  in
  Eio.Switch.run @@ fun sw ->
  let stdout_r, stdout_w = Eio_unix.pipe sw in
  let stderr_r, stderr_w = Eio_unix.pipe sw in
  let stdin_r, stdin_w = Eio_unix.pipe sw in
  let child =
    Eio.Process.spawn ~sw proc_mgr ~env:(curl_env ())
      ~stdin:(stdin_r :> Eio.Flow.source_ty Eio.Resource.t)
      ~stdout:(stdout_w :> Eio.Flow.sink_ty Eio.Resource.t)
      ~stderr:(stderr_w :> Eio.Flow.sink_ty Eio.Resource.t)
      (prog :: args)
  in
  Eio.Resource.close stdout_w;
  Eio.Resource.close stderr_w;
  if String.length stdin_str > 0 then (
    Eio.Flow.copy
      (Eio.Flow.string_source stdin_str)
      (stdin_w :> Eio.Flow.sink_ty Eio.Resource.t);
    Eio.Resource.close stdin_w
  ) else
    Eio.Resource.close stdin_w;
  Eio.Resource.close stdin_r;
  let buf_out = Buffer.create 256 and buf_err = Buffer.create 64 in
  Eio.Fiber.any
    [
      (fun () ->
        Eio.Fiber.both
          (fun () -> Eio.Flow.copy stdout_r (Eio.Flow.buffer_sink buf_out))
          (fun () -> Eio.Flow.copy stderr_r (Eio.Flow.buffer_sink buf_err)));
      (fun () ->
        Eio.Fiber.yield ();
        ignore (Eio.Process.await child : Eio.Process.exit_status));
    ];
  let status = Eio.Process.await child in
  let status_unix =
    match status with
    | `Exited n -> Unix.WEXITED n
    | `Signaled n -> Unix.WSIGNALED n
  in
  {
    Process_result.status = status_unix;
    stdout = Buffer.contents buf_out;
    stderr = Buffer.contents buf_err;
  }

(** {2 Public API} *)

let is_informational_code status = 100 <= status && status <= 199
let is_redirect_code status = status <= 308 && status >= 300
let has_body t = String.length t.Request.body > 0

let run ?(exe = "curl") ?(args = []) ?(follow_redirects = false) req =
  let ( >>= ) = Result.bind in
  (if has_body req && (req.Request.meth = `GET || req.Request.meth = `HEAD) then
     Error (Error.Invalid_request "No body is allowed with GET/HEAD methods")
   else
     Ok req)
  >>= fun req ->
  let args = ("-si" :: Request.to_cmd_args req) @ args in
  let args =
    if follow_redirects then
      "-L" :: args
    else
      args
  in
  let res =
    try
      let pr = run_prog exe args req.body in
      match pr.status with
      | Unix.WEXITED 0 -> Ok pr
      | _ -> Error (Error.Bad_exit pr)
    with e -> Error (Error.Exn e)
  in
  let rec handle_res (res : Process_result.t) =
    match response_of_stdout res.stdout with
    | Ok r ->
      if
        is_informational_code r.code
        || (follow_redirects && is_redirect_code r.code)
      then
        handle_res { res with stdout = r.body }
      else
        Ok r
    | Error e -> Error (Error.Failed_to_read_response (e, res))
  in
  res >>= handle_res

let get ?exe ?args ?headers ?follow_redirects url =
  run ?exe ?args ?follow_redirects (Request.make ?headers ~url ~meth:`GET ())

let head ?exe ?args ?headers ?follow_redirects url =
  run ?exe ?args ?follow_redirects (Request.make ?headers ~url ~meth:`HEAD ())

let delete ?exe ?args ?headers ?follow_redirects url =
  run ?exe ?args ?follow_redirects (Request.make ?headers ~url ~meth:`DELETE ())

let post ?exe ?args ?headers ?body ?follow_redirects url =
  run ?exe ?args ?follow_redirects
    (Request.make ?body ?headers ~url ~meth:`POST ())

let put ?exe ?args ?headers ?body ?follow_redirects url =
  run ?exe ?args ?follow_redirects
    (Request.make ?body ?headers ~url ~meth:`PUT ())

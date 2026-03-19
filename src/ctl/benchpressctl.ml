(** benchpressctl — CLI client for the benchpress server Twirp API *)

open Printf

(* ── Twirp client transport via curly ──────────────────────────────────── *)

module Curly_transport = struct
  module IO = struct
    type 'a t = 'a
    let return x = x
    let ( let* ) x f = f x
  end

  type client = { api_key : string }

  let http_post ~headers ~url ~body (c : client) () =
    let headers = ("authorization", "Bearer " ^ c.api_key) :: headers in
    let req = Curly.Request.make ~meth:`POST ~url ~headers ~body () in
    match Curly.run req with
    | Ok r ->
      Ok (r.Curly.Response.body, r.Curly.Response.code, r.Curly.Response.headers)
    | Error e -> Error (Format.asprintf "%a" Curly.Error.pp e)
end

module Client = Twirp_core.Client.Make (Curly_transport)

(* ── Helpers ────────────────────────────────────────────────────────────── *)

open Benchpress_api_proto.Benchpress_api

let str_of_status = function
  | Queued -> "queued"
  | Running -> "running"
  | Completed -> "completed"
  | Cancelled -> "cancelled"
  | Failed -> "failed"

let json_of_new_job_resp r =
  encode_json_new_job_response r |> Yojson.Basic.to_string

let json_of_status_resp r =
  encode_json_get_job_status_response r |> Yojson.Basic.to_string

let die fmt = ksprintf (fun s -> eprintf "error: %s\n%!" s; exit 1) fmt

type rpc_config = {
  client: Curly_transport.client;
  host: string;
  port: int;
}

let call_rpc cfg rpc req =
  match Client.call ~encoding:`JSON ~host:cfg.host ~port:cfg.port cfg.client rpc req with
  | Ok r -> r
  | Error e ->
    die "%s" (Format.asprintf "%a" Twirp_core.Error.pp_error e)

(* ── Common CLI terms ───────────────────────────────────────────────────── *)

let parse_server s =
  match String.rindex_opt s ':' with
  | None -> s, 8080
  | Some i ->
    let host = String.sub s 0 i in
    let port_str = String.sub s (i + 1) (String.length s - i - 1) in
    host, (match int_of_string_opt port_str with Some p -> p | None -> 8080)

let server_term =
  let open Cmdliner in
  Arg.(
    value
    & opt string "localhost:8080"
    & info [ "server" ]
        ~env:(Cmd.Env.info "BENCHPRESS_SERVER")
        ~doc:"Server address as HOST:PORT"
        ~docv:"HOST:PORT")

let api_key_term =
  let open Cmdliner in
  Arg.(
    required
    & opt (some string) None
    & info [ "api-key" ]
        ~env:(Cmd.Env.info "BENCHPRESS_API_KEY")
        ~doc:"API key (Bearer token)"
        ~docv:"KEY")

let common_term =
  let open Cmdliner in
  Term.(
    const (fun server api_key ->
        let host, port = parse_server server in
        { client = { Curly_transport.api_key }; host; port })
    $ server_term
    $ api_key_term)

(* ── Status display helper ──────────────────────────────────────────────── *)

let display_status ~json resp =
  if json then
    print_string (json_of_status_resp resp)
  else
    match resp.status with
    | Running ->
      printf "status: running (%ld%%)\n" resp.progress_percent
    | Completed ->
      printf "completed: %s\n" resp.result_file
    | Queued ->
      printf "status: queued\n"
    | Cancelled ->
      eprintf "job cancelled\n"
    | Failed ->
      eprintf "job failed\n"

(* ── Polling loop ───────────────────────────────────────────────────────── *)

let poll cfg json job_id =
  let interval = 2.0 in
  let rec loop () =
    Unix.sleepf interval;
    let r = call_rpc cfg BenchpressApi.Client.getJobStatus
        (make_get_job_status_request ~job_id ()) in
    match r.status with
    | Queued | Running ->
      if not json then
        eprintf "status: %s (%ld%%)\n%!" (str_of_status r.status) r.progress_percent;
      loop ()
    | Completed -> display_status ~json r; exit 0
    | Cancelled -> display_status ~json r; exit 1
    | Failed -> display_status ~json r; exit 1
  in
  loop ()

(* ── run subcommand ─────────────────────────────────────────────────────── *)

module Cmd_run = struct
  type params = {
    provers: string list; [@opt_all] [@names ["prover"; "p"]] [@non_empty] [@docv "PROVER"]
    paths: string list;   [@opt_all] [@names ["path"]]        [@non_empty] [@docv "PATH"]
    timeout: int option;  [@names ["timeout"; "t"]]
    memory: int option;   [@names ["memory"; "m"]]
    name: string option;  [@names ["name"]]
    wait: bool;           [@names ["wait"; "w"]]
    json: bool;
  } [@@deriving subliner]

  let run cfg p =
    let req =
      make_new_job_request
        ~provers:p.provers
        ~paths:p.paths
        ?timeout_s:(Option.map Int32.of_int p.timeout)
        ?memory_mb:(Option.map Int32.of_int p.memory)
        ?output_name:p.name
        ()
    in
    let resp = call_rpc cfg BenchpressApi.Client.newJob req in
    if p.wait then (
      if not p.json then
        eprintf "job submitted: %s\n%!" resp.job_id;
      poll cfg p.json resp.job_id
    ) else (
      if p.json then print_string (json_of_new_job_resp resp)
      else printf "%s\n" resp.job_id
    )

  let cmd =
    let open Cmdliner in
    let info = Cmd.info "run" ~doc:"Submit a benchmarking job" in
    Cmd.v info
      Term.(
        const run
        $ common_term
        $ params_cmdliner_term ())
end

(* ── status subcommand ──────────────────────────────────────────────────── *)

module Cmd_status = struct
  type params = {
    job_id: string; [@pos 0] [@docv "JOB_ID"]
    json: bool;
  } [@@deriving subliner]

  let run cfg p =
    let r =
      call_rpc cfg
        BenchpressApi.Client.getJobStatus
        (make_get_job_status_request ~job_id:p.job_id ())
    in
    display_status ~json:p.json r

  let cmd =
    let open Cmdliner in
    let info = Cmd.info "status" ~doc:"Query job status" in
    Cmd.v info
      Term.(
        const run
        $ common_term
        $ params_cmdliner_term ())
end

(* ── cancel subcommand ──────────────────────────────────────────────────── *)

module Cmd_cancel = struct
  type params = {
    job_id: string; [@pos 0] [@docv "JOB_ID"]
  } [@@deriving subliner]

  let run cfg p =
    let _r =
      call_rpc cfg
        BenchpressApi.Client.cancelJob
        (make_cancel_job_request ~job_id:p.job_id ())
    in
    printf "job cancelled\n"

  let cmd =
    let open Cmdliner in
    let info = Cmd.info "cancel" ~doc:"Cancel a running job" in
    Cmd.v info
      Term.(
        const run
        $ common_term
        $ params_cmdliner_term ())
end

(* ── entry point ────────────────────────────────────────────────────────── *)

let () =
  let open Cmdliner in
  let info =
    Cmd.info "benchpressctl" ~version:"dev"
      ~doc:"CLI client for the benchpress server API"
  in
  let default =
    Term.(ret (const (fun () -> `Help (`Pager, None)) $ const ()))
  in
  exit
    (Cmd.eval
       (Cmd.group info ~default
          [ Cmd_run.cmd; Cmd_status.cmd; Cmd_cancel.cmd ]))

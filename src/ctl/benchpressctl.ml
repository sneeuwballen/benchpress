(** benchpressctl — CLI client for the benchpress server Twirp API *)

open Printf
module Log = (val Logs.src_log (Logs.Src.create "benchpressctl"))

let setup_logs (lvl : Logs.level option) =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level ~all:true lvl;
  Log.info (fun k -> k "logging initialized")

(* ── Twirp client transport via curly ──────────────────────────────────── *)

module Curly_transport = struct
  module IO = struct
    type 'a t = 'a

    let return x = x
    let ( let* ) x f = f x
  end

  type client = { api_key: string option }

  let http_post ~headers ~url ~body (c : client) () =
    let headers =
      match c.api_key with
      | Some key -> ("authorization", "Bearer " ^ key) :: headers
      | None -> headers
    in
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

(** Format a [stat] as "name:value". *)
let pp_stat s = Printf.sprintf "%s:%ld" s.name s.value

let json_of_new_job_resp r =
  encode_json_new_job_response r |> Yojson.Basic.to_string

let json_of_status_resp r =
  encode_json_get_job_status_response r |> Yojson.Basic.to_string

let die fmt =
  ksprintf
    (fun s ->
      eprintf "error: %s\n%!" s;
      exit 1)
    fmt

type rpc_config = { host: string; port: int; api_key: string option }

let call_rpc cfg rpc req =
  let client = { Curly_transport.api_key = cfg.api_key } in
  match
    Client.call ~encoding:`JSON ~host:cfg.host ~port:cfg.port client rpc req
  with
  | Ok r -> r
  | Error e -> die "%s" (Format.asprintf "%a" Twirp_core.Error.pp_error e)

let parse_server s =
  match String.rindex_opt s ':' with
  | None -> s, 8080
  | Some i ->
    let host = String.sub s 0 i in
    let port_str = String.sub s (i + 1) (String.length s - i - 1) in
    host, Option.value ~default:8080 (int_of_string_opt port_str)

(* ── Status display helpers ──────────────────────────────────────────────── *)

let json_of_list_jobs_resp r =
  encode_json_list_jobs_response r |> Yojson.Basic.to_string

let display_job_list ~json resp =
  if json then
    print_string (json_of_list_jobs_resp resp)
  else
    List.iter
      (fun (e : job_entry) ->
        printf "%s  %s" e.job_id (str_of_status e.status);
        (match e.status with
        | Running -> printf " (%ld%%)" e.progress_percent
        | Completed -> printf "  %s" e.result_file
        | _ -> ());
        printf "\n")
      resp.jobs

let display_status ~json resp =
  if json then
    print_string (json_of_status_resp resp)
  else (
    match resp.status with
    | Running -> printf "status: running (%ld%%)\n" resp.progress_percent
    | Completed -> printf "completed: %s\n" resp.result_file
    | Queued -> printf "status: queued\n"
    | Cancelled -> eprintf "job cancelled\n"
    | Failed -> eprintf "job failed\n"
  )

(* ── Polling loop ───────────────────────────────────────────────────────── *)

let poll cfg json job_id =
  let interval = 2.0 in
  let rec loop () =
    Unix.sleepf interval;
    let r =
      call_rpc cfg BenchpressApi.Client.getJobStatus
        (make_get_job_status_request ~job_id ())
    in
    match r.status with
    | Queued | Running ->
      if not json then
        eprintf "status: %s (%ld%%)\n%!" (str_of_status r.status)
          r.progress_percent;
      loop ()
    | Completed ->
      display_status ~json r;
      exit 0
    | Cancelled ->
      display_status ~json r;
      exit 1
    | Failed ->
      display_status ~json r;
      exit 1
  in
  loop ()

(* ── run subcommand ─────────────────────────────────────────────────────── *)

module Cmd_run = struct
  type params = {
    provers: string list;
        [@opt_all] [@names [ "prover"; "p" ]] [@non_empty] [@docv "PROVER"]
    paths: string list;
        [@opt_all] [@names [ "path" ]] [@non_empty] [@docv "PATH"]
    timeout: int option; [@names [ "timeout"; "t" ]]
    memory: int option; [@names [ "memory"; "m" ]]
    name: string option; [@names [ "name" ]]
    wait: bool; [@names [ "wait"; "w" ]]
    json: bool;
    server: string;
        [@default "localhost:8889"]
        [@env "BENCHPRESS_SERVER"]
        [@docv "HOST:PORT"]
    api_key: string option; [@env "BENCHPRESS_API_KEY"] [@docv "KEY"]
  }
  [@@deriving subliner]

  let run p =
    Log.info (fun k -> k "run: provers=%a" Fmt.(list string) p.provers);
    let host, port = parse_server p.server in
    let cfg = { host; port; api_key = p.api_key } in
    let req =
      make_new_job_request ~provers:p.provers ~paths:p.paths
        ?timeout_s:(Option.map Int32.of_int p.timeout)
        ?memory_mb:(Option.map Int32.of_int p.memory)
        ?output_name:p.name ()
    in
    let resp = call_rpc cfg BenchpressApi.Client.newJob req in
    if p.wait then (
      if not p.json then eprintf "job submitted: %s\n%!" resp.job_id;
      poll cfg p.json resp.job_id
    ) else if p.json then
      print_string (json_of_new_job_resp resp)
    else
      printf "%s\n" resp.job_id

  let cmd =
    let open Cmdliner in
    let info = Cmd.info "run" ~doc:"Submit a benchmarking job" in
    Cmd.v info Term.(const run $ params_cmdliner_term ())
end

(* ── status subcommand ──────────────────────────────────────────────────── *)

module Cmd_status = struct
  type params = {
    job_id: string option; [@pos 0] [@docv "JOB_ID"]
    json: bool;
    server: string;
        [@default "localhost:8889"]
        [@env "BENCHPRESS_SERVER"]
        [@docv "HOST:PORT"]
    api_key: string option; [@env "BENCHPRESS_API_KEY"] [@docv "KEY"]
  }
  [@@deriving subliner]

  let run p =
    let host, port = parse_server p.server in
    let cfg = { host; port; api_key = p.api_key } in
    match p.job_id with
    | Some job_id ->
      let r =
        call_rpc cfg BenchpressApi.Client.getJobStatus
          (make_get_job_status_request ~job_id ())
      in
      display_status ~json:p.json r
    | None ->
      let r = call_rpc cfg BenchpressApi.Client.listJobs () in
      display_job_list ~json:p.json r

  let cmd =
    let open Cmdliner in
    let info =
      Cmd.info "status" ~doc:"Query job status (list all if no JOB_ID given)"
    in
    Cmd.v info Term.(const run $ params_cmdliner_term ())
end

(* ── cancel subcommand ──────────────────────────────────────────────────── *)

module Cmd_cancel = struct
  type params = {
    job_id: string; [@pos 0] [@docv "JOB_ID"]
    server: string;
        [@default "localhost:8889"]
        [@env "BENCHPRESS_SERVER"]
        [@docv "HOST:PORT"]
    api_key: string option; [@env "BENCHPRESS_API_KEY"] [@docv "KEY"]
  }
  [@@deriving subliner]

  let run p =
    let host, port = parse_server p.server in
    let cfg = { host; port; api_key = p.api_key } in
    let _r =
      call_rpc cfg BenchpressApi.Client.cancelJob
        (make_cancel_job_request ~job_id:p.job_id ())
    in
    printf "job cancelled\n"

  let cmd =
    let open Cmdliner in
    let info = Cmd.info "cancel" ~doc:"Cancel a running job" in
    Cmd.v info Term.(const run $ params_cmdliner_term ())
end

(* ── listen subcommand ──────────────────────────────────────────────────── *)

module Cmd_listen = struct
  module Api = Benchpress_api_proto.Benchpress_api

  type params = {
    server: string; [@default "localhost:4222"] [@docv "HOST:PORT"]
        (** NATS server address *)
  }
  [@@deriving subliner]

  let run p =
    let host, port =
      match String.rindex_opt p.server ':' with
      | None -> p.server, 4222
      | Some i ->
        ( String.sub p.server 0 i,
          (match
             int_of_string_opt
               (String.sub p.server (i + 1) (String.length p.server - i - 1))
           with
          | Some port -> port
          | None -> 4222) )
    in
    let host_s =
      try Unix.string_of_inet_addr (Unix.inet_addr_of_string host)
      with Failure _ ->
        let entry = Unix.gethostbyname host in
        if Array.length entry.Unix.h_addr_list = 0 then
          failwith (Printf.sprintf "no address for host %S" host);
        Unix.string_of_inet_addr entry.Unix.h_addr_list.(0)
    in
    Log.info (fun k -> k "connecting to NATS at %s:%d" host_s port);
    Eio_main.run @@ fun env ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    let subject = [ "benchpress"; "progress"; ">" ] in
    Eio.Switch.run @@ fun sw ->
    Nats.with_connect ~sw ~net ~host:host_s ~port () (fun nats ->
        Log.info (fun k -> k "subscribed to %s" (String.concat "." subject));
        let last_print = ref 0.0 in
        let _sub =
          Nats.sub nats ~sw ~subject (fun msg ->
              let now = Unix.gettimeofday () in
              match
                Api.decode_json_progress_report
                  (Yojson.Basic.from_string msg.payload)
              with
              | report ->
                Log.debug (fun k ->
                    k "progress: %s %ld/%ld" report.Api.uuid
                      report.Api.done_tasks report.Api.total_tasks);
                let should_print = now -. !last_print >= 10.0 in
                if should_print then last_print := now;
                if should_print || report.Api.finished then (
                  let pct =
                    if report.Api.total_tasks = 0l then
                      0
                    else
                      Int32.to_int report.Api.done_tasks
                      * 100
                      / Int32.to_int report.Api.total_tasks
                  in
                  let status =
                    if report.Api.finished then
                      "done"
                    else
                      "running"
                  in
                  let stat_l_str =
                    if report.Api.stat_l <> [] then
                      String.concat " " (List.map pp_stat report.Api.stat_l)
                    else
                      ""
                  in
                  let stats_str =
                    if report.Api.stats <> "" then
                      Printf.sprintf " %s" report.Api.stats
                    else
                      ""
                  in
                  Printf.printf "[%s] %s %d%%%s%s\n%!" report.Api.uuid status
                    pct
                    (if stat_l_str <> "" then
                       Printf.sprintf " %s" stat_l_str
                     else
                       "")
                    stats_str
                )
              | exception exn ->
                Printf.eprintf "warning: invalid progress message: %s\n%!"
                  (Printexc.to_string exn))
        in
        Eio.Time.sleep clock infinity)

  let cmd =
    let open Cmdliner in
    let info = Cmd.info "listen" ~doc:"Listen for progress events via NATS" in
    Cmd.v info Term.(const run $ params_cmdliner_term ())
end

(* ── entry point ────────────────────────────────────────────────────────── *)

let get_log_level () =
  match Sys.getenv_opt "LOG_LEVEL" with
  | None -> Some Logs.Warning
  | Some "debug" | Some "DEBUG" -> Some Logs.Debug
  | Some "info" | Some "INFO" -> Some Logs.Info
  | Some "warning" | Some "WARNING" -> Some Logs.Warning
  | Some "error" | Some "ERROR" -> Some Logs.Error
  | Some "app" | Some "APP" -> Some Logs.App
  | Some s ->
    Printf.eprintf "benchpressctl: invalid LOG_LEVEL=%S, using Warning\n%!" s;
    Some Logs.Warning

let () =
  setup_logs (get_log_level ());
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
          [ Cmd_run.cmd; Cmd_status.cmd; Cmd_cancel.cmd; Cmd_listen.cmd ]))

(* run tests, or compare results *)
open Common
module T = Test
module H = Tiny_httpd
module U = Tiny_httpd.Util
module PB = PrintBox
module Log = (val Logs.src_log (Logs.Src.create "benchpress-serve"))

let handle_health (self : Server_common.t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "health" @/ return)
    (fun _req -> H.Response.make_string @@ Ok "ok")

let handle_assets self : unit =
  let mk_path p' ctype value =
    let h = Digest.to_hex (Digest.string value) in
    let etag = Printf.sprintf {|"%s"|} h in
    H.add_route_handler self ~meth:`GET
      H.Route.(exact p' @/ return)
      (fun req ->
        let inm = H.Request.get_header req "If-None-Match" in
        if inm = Some etag then (
          Log.debug (fun k -> k "cached object (etag: %s)" etag);
          H.Response.make_raw ~code:304 ""
        ) else
          H.Response.make_string
            ~headers:
              [
                "content-type", ctype; "Etag", etag; "Cache-Control", "no-cache";
              ]
            (Ok value))
  in
  mk_path "css" "text/css" Web_data.css;
  mk_path "js" "text/javascript" Web_data.js;
  mk_path "echarts.js" "text/javascript" Web_data.echarts_js;
  mk_path "htmx-echarts.js" "text/javascript" Web_data.htmx_echarts_js;
  mk_path "htmx.js" "text/javascript" Web_data.htmx_js;
  mk_path "favicon.png" "media/png" Web_data.favicon;
  ()

(** {2 External progress reporting endpoint + HTMX status fragment} *)

let handle_ext_progress (self : Server_common.t) : unit =
  H.add_route_handler self.server ~meth:`POST
    H.Route.(exact "api" @/ exact "progress" @/ return)
  @@ fun req ->
  let body = H.Request.body req in
  let json_headers = H.Headers.([] |> set "content-type" "application/json") in
  let report =
    try
      Some
        (Benchpress_api_proto.Benchpress_api.decode_json_progress_report
           (Yojson.Basic.from_string body))
    with _ -> None
  in
  match report with
  | None ->
    H.Response.make_string ~code:400 ~headers:json_headers
      (Ok {|{"error":"invalid json"}|})
  | Some r ->
    let module Api = Benchpress_api_proto.Benchpress_api in
    if r.Api.uuid = "" then
      H.Response.make_string ~code:400 ~headers:json_headers
        (Ok {|{"error":"missing uuid"}|})
    else (
      Server_common.Ext_jobs.apply_report self.ext_jobs
        ~now:(Unix.gettimeofday ()) r;
      H.Response.make_string ~code:200 ~headers:json_headers
        (Ok {|{"ok":true}|})
    )

(** Build an HTMX fragment with progress bars from external jobs and optionally
    from the current TaskQueue job. *)
let build_progress_bars (self : Server_common.t) ~(now : float) : string =
  let open Server_common in
  let open Html in
  let module Api = Benchpress_api_proto.Benchpress_api in
  Ext_jobs.expire self.ext_jobs ~now;
  let format_start_ts_iso ts =
    let t = Unix.gmtime ts in
    spf "%04d-%02d-%02dT%02d:%02d:%02d" (t.tm_year + 1900) (t.tm_mon + 1)
      t.tm_mday t.tm_hour t.tm_min t.tm_sec
  in
  let result_url_of_job ~uuid ~start_ts =
    match Uuidm.of_string uuid with
    | None -> "/"
    | Some u ->
      let ts_str =
        match Ptime.of_float_s start_ts with
        | None -> spf "<time %.1fs>" start_ts
        | Some t -> Human.datetime_compact t
      in
      uri_show (spf "res-%s-%s.sqlite" ts_str (Uuidm.to_string u))
  in
  let pill_of_active (a : Api.active_item) =
    span
      [
        A.class_ "badge pill-cream";
        A.title
          (spf "%s on %s (%.0fs)" a.Api.prover a.Api.file a.Api.running_time);
      ]
      [
        txt
          (spf "%s:%s (%.0fs)" a.Api.prover
             (Filename.basename a.Api.file)
             a.Api.running_time);
      ]
  in
  let card_of_job ~uuid ~job_url ~start_ts ~done_ ~total ~elapsed ~pct ~active =
    let uuid_short = String.sub uuid 0 8 in
    let start_iso = format_start_ts_iso start_ts in
    let total_str =
      if total > 0 then
        string_of_int total
      else
        "?"
    in
    div
      [ A.class_ "card shadow-sm mb-1" ]
      [
        div
          [ A.class_ "card-body py-1 px-2" ]
          [
            div
              [ A.class_ "d-flex justify-content-between align-items-center" ]
              [
                a
                  [
                    A.class_ "fw-bold text-decoration-none";
                    A.href job_url;
                    "data-utc-date", start_iso;
                    A.title (spf "started: %s" start_iso);
                  ]
                  [
                    (if pct < 100 then
                       span
                         [
                           A.class_ "spinner-border spinner-border-sm me-1";
                           A.style "width:0.8em;height:0.8em";
                         ]
                         []
                     else
                       txt "");
                    txt (spf "Job %s\u{2026}" uuid_short);
                  ];
                small
                  [ A.class_ "text-muted" ]
                  [
                    txt
                      (spf "%d/%s (%s)" done_ total_str
                         (Human.human_duration elapsed));
                  ];
              ];
            div
              [ A.class_ "progress"; A.style "height: 4px" ]
              [
                div
                  [
                    A.class_
                      "progress-bar progress-bar-striped progress-bar-animated";
                    A.style (spf "width:%d%%" pct);
                  ]
                  [];
              ];
            (if active <> [] then
               div
                 [ A.class_ "d-flex flex-wrap gap-1 mt-1" ]
                 (List.map pill_of_active active)
             else
               div [] []);
          ];
      ]
  in
  (* Collect cards from external jobs *)
  let ext_cards =
    Ext_jobs.all_active self.ext_jobs
    |> List.map (fun (j : Ext_jobs.job) ->
           let r = j.report in
           let total = Int32.to_int r.Api.total_tasks in
           let done_ = Int32.to_int r.Api.done_tasks in
           let pct =
             if total > 0 then
               done_ * 100 / total
             else
               0
           in
           let elapsed = now -. r.Api.start_ts in
           let job_url =
             result_url_of_job ~uuid:r.Api.uuid ~start_ts:r.Api.start_ts
           in
           card_of_job ~uuid:r.Api.uuid ~job_url ~start_ts:r.Api.start_ts ~done_
             ~total ~elapsed ~pct ~active:r.Api.active)
  in
  (* Also include the local TaskQueue job, if any *)
  let local_cards =
    match Task_queue.cur_job self.task_q with
    | None -> []
    | Some j ->
      let pct = Task_queue.Job.percent_completion j in
      let elapsed = Task_queue.Job.time_elapsed j in
      let uuid_local = Task_queue.Job.uuid j in
      let start_ts_local = now -. elapsed in
      let job_url =
        result_url_of_job ~uuid:uuid_local ~start_ts:start_ts_local
      in
      [
        card_of_job ~uuid:uuid_local ~job_url ~start_ts:start_ts_local
          ~done_:pct ~total:100 ~elapsed ~pct ~active:[];
      ]
  in
  let cards = ext_cards @ local_cards in
  if cards = [] then
    div [ A.id "ext-jobs-status" ] [] |> to_string_elt
  else
    div
      [
        A.id "ext-jobs-status";
        "hx-get", "/api/ext-jobs-status/";
        "hx-trigger", "every 4s";
        "hx-swap", "outerHTML";
        A.class_ "mb-2";
      ]
      cards
    |> to_string_elt

let handle_ext_jobs_status (self : Server_common.t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "api" @/ exact "ext-jobs-status" @/ return)
  @@ fun _req ->
  let now = Unix.gettimeofday () in
  let html = build_progress_bars self ~now in
  H.Response.make_string ~headers:Server_common.default_html_headers (Ok html)

(** {2 NATS progress subscription} *)

(** Subscribe to NATS for benchpress.progress.> events and feed them into the
    Ext_jobs table so they appear as progress bars in the web UI. *)
let handle_nats_progress (self : Server_common.t) ~(sw : Eio.Switch.t) ~net :
    unit =
  match Definitions.option_nats_server self.Server_common.defs with
  | None -> ()
  | Some addr ->
    let host, port =
      match String.rindex_opt addr ':' with
      | None -> addr, 4222
      | Some i ->
        ( String.sub addr 0 i,
          (match
             int_of_string_opt
               (String.sub addr (i + 1) (String.length addr - i - 1))
           with
          | Some p -> p
          | None -> 4222) )
    in
    let host_s =
      try Nats.resolve_host host
      with _ ->
        Log.warn (fun k -> k "could not resolve NATS host %S" host);
        ""
    in
    if host_s = "" then
      ()
    else (
      let subject = [ "benchpress"; "progress"; ">" ] in
      try
        (* Use [connect] directly so the connection lives on the switch.
           [with_connect] would close it as soon as the callback returns. *)
        let nats = Nats.connect ~sw ~net ~host:host_s ~port () in
        Eio.Switch.on_release sw (fun () -> Nats.close nats);
        Log.info (fun k ->
            k "subscribed to NATS progress on %s:%d (%s)" host_s port addr);
        let _sub =
          Nats.sub nats ~sw ~subject (fun (msg : Nats.msg) ->
              let now = Unix.gettimeofday () in
              match
                Benchpress_api_proto.Benchpress_api.decode_json_progress_report
                  (Yojson.Basic.from_string msg.Nats.payload)
              with
              | report ->
                Server_common.Ext_jobs.apply_report self.ext_jobs ~now report;
                let module Api = Benchpress_api_proto.Benchpress_api in
                Log.debug (fun k ->
                    k "nats progress: %s %ld/%ld"
                      (match report.Api.uuid with
                      | "" -> "?"
                      | u -> String.sub u 0 8)
                      report.Api.done_tasks report.Api.total_tasks)
              | exception exn ->
                Log.warn (fun k ->
                    k "invalid NATS progress message: %s"
                      (Printexc.to_string exn)))
        in
        ()
      with exn ->
        Log.warn (fun k ->
            k "could not connect to NATS at %s:%d: %s" host_s port
              (Printexc.to_string exn))
    )

let handle_file (self : Server_common.t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "get-file" @/ string_urlencoded @/ return)
    (fun file _req ->
      Log.debug (fun k -> k "get-file: `%s`" file);
      let bytes =
        if file = "prelude" then
          H.IO.Input.of_string Static_data.builtin_config (* magic file! *)
        else (
          try H.IO.Input.of_in_channel @@ open_in file
          with e ->
            H.Response.fail_raise ~code:404
              "cannot open file %S:\n\
               %s\n\n\
               The benchmark might not be present on this machine."
              file (Printexc.to_string e)
        )
      in
      H.Response.make_raw_stream
        ~headers:[ "Content-Type", "text/plain" ]
        ~code:200 bytes)

(** {2 Embedded web server} *)

module Cmd = struct
  let main ?(local_only = false) ?port ~allow_delete ~allow_localhost ~log_lvl
      ~(stdenv : Eio_unix.Stdenv.base) (defs : Definitions.t) () =
    try
      Server_common.Logger.setup log_lvl;
      let addr =
        if local_only then
          "127.0.0.1"
        else
          "0.0.0.0"
      in
      Eio.Switch.run @@ fun sw ->
      let server =
        Tiny_httpd_eio.create ~stdenv ~sw
          ~middlewares:[ `Stage 1, Server_common.trace_middleware ]
          ~max_connections:32 ~addr ?port ()
      in

      let prometheus = Tiny_httpd_prometheus.(global) in
      Tiny_httpd_prometheus.instrument_server server prometheus;
      Tiny_httpd_prometheus.GC_metrics.create_and_update_before_emit prometheus;

      let data_dir = Misc.data_dir () in
      let meta_cache =
        Meta_cache.create ~sw ~path:(Filename.concat data_dir "meta.sqlite3")
      in
      Mirage_crypto_rng_unix.use_default ();
      let auth = Auth.create (Auth.default_path ()) in
      let self =
        {
          Server_common.defs;
          server;
          data_dir;
          task_q = Task_queue.create ~defs ();
          meta_cache;
          allow_delete;
          allow_localhost;
          auth;
          ext_jobs = Server_common.Ext_jobs.create ();
        }
      in
      let mcp_reg = Benchpress_mcp.Mcp.create () in
      Benchpress_mcp.Mcp_tools.register_all ~reg:mcp_reg ~data_dir;
      (* fiber to execute tasks *)
      Eio.Fiber.fork ~sw (fun () -> Task_queue.loop self.task_q);
      (* subscribe to NATS progress events if configured *)
      let net = Eio.Stdenv.net stdenv in
      handle_nats_progress self ~sw ~net;
      (* maybe serve the API *)
      Printf.printf "listen on http://localhost:%d/\n%!" (H.port server);
      P_root.handle_root self;
      handle_health self;
      P_root.handle_list_benchs self;
      P_root.handle_file_summary self;
      handle_assets server;
      P_show.handle_show self;
      P_meta.handle_user_meta_get self;
      P_meta.handle_user_meta_post self;
      P_meta.handle_user_meta_delete self;
      P_echarts.handle_show_echarts self;
      P_evolution.handle_evolution self;
      P_show.handle_prover_in self;
      P_errors.handle_show_errors self;
      P_errors.handle_show_invalid self;
      P_detailed.handle_show_as_table self;
      P_detailed.handle_show_detailed self;
      P_detailed.handle_show_single self;
      P_detailed.handle_show_csv self;
      P_tasks.handle_tasks self;
      P_provers.handle_provers self;
      P_tasks.handle_run self;
      P_tasks.handle_job_interrupt self;
      P_compare.handle_compare self;
      P_compare.handle_compare2 self;
      if allow_delete then P_delete.handle_delete self;
      handle_file self;
      (let mcp_handler req =
         let body = H.Request.body req in
         let resp_body = Benchpress_mcp.Mcp.handle_request mcp_reg body in
         H.Response.make_string
           ~headers:H.Headers.([] |> set "content-type" "application/json")
           (Ok resp_body)
       in
       H.add_route_handler self.server ~meth:`POST
         H.Route.(exact "mcp" @/ return)
         mcp_handler);
      handle_ext_progress self;
      handle_ext_jobs_status self;
      Api_handler.register ~allow_localhost ~auth:self.auth ~task_q:self.task_q
        ~defs:self.defs ~data_dir:self.data_dir ~http_server:self.server;
      H.run server |> CCResult.map_err Error.of_exn
    with e -> Error (Error.of_exn e)

  (* sub-command to serve the web UI *)
  let cmd ~stdenv =
    let open Cmdliner in
    let port =
      Arg.(
        value
        & opt (some int) None
        & info [ "p"; "port" ] ~doc:"port to listen on")
    and local_only =
      Arg.(value & flag & info [ "local-only" ] ~doc:"only listen on localhost")
    and allow_delete =
      Arg.(
        value & opt bool false
        & info [ "allow-delete" ] ~doc:"allow deletion of files")
    and allow_localhost =
      Arg.(
        value & flag
        & info [ "allow-localhost" ]
            ~doc:"allow requests from localhost without API key")
    and defs = Bin_utils.definitions_term in
    let doc = "serve embedded web UI on given port" in
    let aux (log_lvl, defs) port local_only allow_delete allow_localhost () =
      main ?port ~local_only ~allow_delete ~allow_localhost ~stdenv defs
        ~log_lvl ()
    in
    ( Term.(
        const aux $ defs $ port $ local_only $ allow_delete $ allow_localhost
        $ const ()),
      Cmd.info ~doc "serve" )
end

(** {2 Admin subcommands} *)
module Admin = struct
  open Cmdliner

  let resolve_auth_db path =
    if path = "" then
      Auth.default_path ()
    else
      path

  type user_create_params = {
    email: string; [@names [ "email" ]] [@docv "EMAIL"]  (** user email *)
    auth_db: string; [@names [ "auth-db" ]] [@default ""]  (** path to auth DB *)
  }
  [@@deriving subliner]
  (** user create *)

  let user_create_run (p : user_create_params) =
    let db = Auth.create (resolve_auth_db p.auth_db) in
    match Auth.create_user db ~email:p.email with
    | Ok uuid -> Printf.printf "Created user: %s\n%!" uuid
    | Error msg ->
      Printf.eprintf "Error: %s\n%!" msg;
      exit 1

  let user_create_cmd =
    let doc = "create a new user" in
    Cmd.v (Cmd.info ~doc "create")
      Term.(const user_create_run $ user_create_params_cmdliner_term ())

  type user_list_params = {
    auth_db: string; [@names [ "auth-db" ]] [@default ""]  (** path to auth DB *)
  }
  [@@deriving subliner]
  (** user list *)

  let user_list_run (p : user_list_params) =
    let db = Auth.create (resolve_auth_db p.auth_db) in
    let users = Auth.list_users db in
    List.iter
      (fun (id, email, created_at) ->
        Printf.printf "%s\t%s\t%s\n" id email created_at)
      users

  let user_list_cmd =
    let doc = "list all users" in
    Cmd.v (Cmd.info ~doc "list")
      Term.(const user_list_run $ user_list_params_cmdliner_term ())

  let user_cmd =
    let doc = "manage users" in
    Cmd.group (Cmd.info ~doc "user") [ user_create_cmd; user_list_cmd ]

  type api_key_create_params = {
    user: string; [@names [ "user" ]] [@docv "UUID"]  (** user UUID *)
    auth_db: string; [@names [ "auth-db" ]] [@default ""]  (** path to auth DB *)
  }
  [@@deriving subliner]
  (** api-key create *)

  let api_key_create_run (p : api_key_create_params) =
    Mirage_crypto_rng_unix.use_default ();
    let db = Auth.create (resolve_auth_db p.auth_db) in
    match Auth.create_api_key db ~user_id:p.user with
    | Ok key -> Printf.printf "Created API key: %s\n%!" key
    | Error msg ->
      Printf.eprintf "Error: %s\n%!" msg;
      exit 1

  let api_key_create_cmd =
    let doc = "create a new API key for a user" in
    Cmd.v (Cmd.info ~doc "create")
      Term.(const api_key_create_run $ api_key_create_params_cmdliner_term ())

  type api_key_revoke_params = {
    key: string; [@names [ "key" ]] [@docv "HEX"]  (** API key hex *)
    auth_db: string; [@names [ "auth-db" ]] [@default ""]  (** path to auth DB *)
  }
  [@@deriving subliner]
  (** api-key revoke *)

  let api_key_revoke_run (p : api_key_revoke_params) =
    let db = Auth.create (resolve_auth_db p.auth_db) in
    Auth.revoke_api_key db ~key:p.key;
    Printf.printf "Revoked key: %s\n%!" p.key

  let api_key_revoke_cmd =
    let doc = "revoke an API key" in
    Cmd.v (Cmd.info ~doc "revoke")
      Term.(const api_key_revoke_run $ api_key_revoke_params_cmdliner_term ())

  type api_key_list_params = {
    user: string; [@names [ "user" ]] [@docv "UUID"]  (** user UUID *)
    auth_db: string; [@names [ "auth-db" ]] [@default ""]  (** path to auth DB *)
  }
  [@@deriving subliner]
  (** api-key list *)

  let api_key_list_run (p : api_key_list_params) =
    let db = Auth.create (resolve_auth_db p.auth_db) in
    let keys = Auth.list_api_keys db ~user_id:p.user in
    List.iter
      (fun (key, created_at) -> Printf.printf "%s\t%s\n" key created_at)
      keys

  let api_key_list_cmd =
    let doc = "list API keys for a user" in
    Cmd.v (Cmd.info ~doc "list")
      Term.(const api_key_list_run $ api_key_list_params_cmdliner_term ())

  let api_key_cmd =
    let doc = "manage API keys" in
    Cmd.group (Cmd.info ~doc "api-key")
      [ api_key_create_cmd; api_key_revoke_cmd; api_key_list_cmd ]
end

let () =
  Opentelemetry.Globals.service_name := "benchpress";
  Opentelemetry.Globals.service_namespace := Some "c-cube";
  ()

let () =
  let@ () = Opentelemetry_client_ocurl.with_setup () in
  Opentelemetry_trace.setup ();
  Opentelemetry.Gc_metrics.setup ~min_interval_s:60 ();
  let@ stdenv = Eio_posix.run in
  Trace_eio.setup ();
  let proc_mgr = Eio.Stdenv.process_mgr stdenv in
  let@ () = Run_proc.with_proc_mgr proc_mgr in
  let serve_t, serve_i = Cmd.cmd ~stdenv in
  (* wrap serve result: (unit, Error.t) result -> unit *)
  let serve_t' =
    Cmdliner.Term.(
      const (function
        | Ok () -> ()
        | Error e ->
          print_endline ("error: " ^ Error.show e);
          Stdlib.exit 1)
      $ serve_t)
  in
  let serve_cmd = Cmdliner.Cmd.v serve_i serve_t' in
  let group =
    Cmdliner.Cmd.group
      (Cmdliner.Cmd.info ~version:"dev" "benchpress-server")
      [ serve_cmd; Admin.user_cmd; Admin.api_key_cmd ]
  in
  let@ _sp = Common.with_span ~__FILE__ ~__LINE__ "cmdliner.eval" in
  match Cmdliner.Cmd.eval group with
  | 0 -> ()
  | n -> exit n

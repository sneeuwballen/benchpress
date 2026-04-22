(** API handler: registers Twirp service handlers with tiny_httpd. We copy and
    extend twirp_tiny_httpd's handler/add_service so that [f] receives the raw
    HTTP request (and thus req.meta, where auth stores the user_id). *)

module Log = (val Logs.src_log (Logs.Src.create "benchpress.api"))
module Api = Benchpress_api_proto.Benchpress_api
module H = Tiny_httpd

type state = {
  task_q: Task_queue.t;
  auth: Auth.t;
  defs: Definitions.t;
  data_dir: string;
}

module Error_codes = Twirp_tiny_httpd.Error_codes
module Twirp_error = Twirp_tiny_httpd.Error

exception Fail of Error_codes.t * string option

let fail ?msg code = raise (Fail (code, msg))
let failf code fmt = Format.kasprintf (fun m -> fail code ~msg:m) fmt

let return_error (code : Error_codes.t) msg : H.Response.t =
  let msg =
    match msg with
    | Some m -> m
    | None -> Error_codes.to_descr code
  in
  let code_str, http_code = Error_codes.to_msg_and_code code in
  let body =
    Twirp_error.default_error ~code:code_str ~msg ()
    |> Twirp_error.encode_json_error |> Yojson.Basic.to_string
  in
  H.Response.make_raw
    ~headers:[ "content-type", "application/json" ]
    ~code:http_code body

(** Like [Twirp_tiny_httpd.handler] but [f] also receives the raw HTTP request
    so handlers can read [req.meta] (e.g. the authenticated user_id). *)
type handler =
  | Handler : {
      rpc:
        ( 'req,
          Pbrt_services.Value_mode.unary,
          'res,
          Pbrt_services.Value_mode.unary )
        Pbrt_services.Server.rpc;
      f: string H.Request.t -> 'req -> 'res;
    }
      -> handler

let mk_handler rpc f : handler = Handler { rpc; f }

let handle_rpc (Handler { rpc; f }) (http_req : string H.Request.t) :
    H.Response.t =
  try
    (match rpc.req_mode, rpc.res_mode with
    | Pbrt_services.Server.Unary, Pbrt_services.Server.Unary -> ()
    | _ -> fail Error_codes.Unimplemented ~msg:"streaming not supported");
    let content_type =
      match H.Request.get_header http_req "content-type" with
      | Some "application/json" -> `JSON
      | Some "application/protobuf" -> `BINARY
      | Some r -> failf Error_codes.Malformed "unknown content-type %S" r
      | None -> fail Error_codes.Malformed ~msg:"missing content-type"
    in
    let proto_req =
      match content_type with
      | `JSON ->
        (try
           rpc.decode_json_req
             (Yojson.Basic.from_string http_req.H.Request.body)
         with _ -> fail Error_codes.Malformed ~msg:"could not decode json")
      | `BINARY ->
        (try rpc.decode_pb_req (Pbrt.Decoder.of_string http_req.H.Request.body)
         with _ -> fail Error_codes.Malformed ~msg:"could not decode protobuf")
    in
    let res = f http_req proto_req in
    match content_type with
    | `JSON ->
      H.Response.make_string
      @@ Ok (Yojson.Basic.to_string @@ rpc.encode_json_res res)
    | `BINARY ->
      let enc = Pbrt.Encoder.create () in
      rpc.encode_pb_res res enc;
      let write out =
        Pbrt.Encoder.write_chunks
          (fun buf i len -> H.IO.Output.output out buf i len)
          enc
      in
      H.Response.make_writer @@ Ok (H.IO.Writer.make ~write ())
  with
  | Fail (code, msg) -> return_error code msg
  | exn ->
    return_error Error_codes.Internal
      (Some (Printf.sprintf "handler raised: %s" (Printexc.to_string exn)))

let add_service ?(prefix = Some "twirp") ~middlewares http_server
    (service : handler Pbrt_services.Server.t) =
  let qualified =
    match service.package with
    | [] -> service.service_name
    | pkg -> Printf.sprintf "%s.%s" (String.concat "." pkg) service.service_name
  in
  List.iter
    (fun (Handler { rpc; _ } as h) ->
      let route =
        let open H.Route in
        let r = exact rpc.name @/ return in
        let r = exact qualified @/ r in
        match prefix with
        | Some p -> exact p @/ r
        | None -> r
      in
      H.add_route_handler http_server ~meth:`POST ~middlewares route
        (handle_rpc h))
    service.handlers

let current_user_exn http_req =
  match Auth_middleware.user_of_req http_req with
  | Some u -> u
  | None -> fail Error_codes.Unauthenticated ~msg:"unauthorized"

let handle_new_job state http_req (req : Api.new_job_request) =
  let user_id = current_user_exn http_req in
  if req.Api.provers = [] then
    fail Error_codes.Invalid_argument ~msg:"provers is empty";
  if req.Api.paths = [] then
    fail Error_codes.Invalid_argument ~msg:"paths is empty";
  let timeout_s =
    let v = Int32.to_int req.Api.timeout_s in
    if v <= 0 then
      None
    else
      Some v
  in
  let memory_mb =
    let v = Int32.to_int req.Api.memory_mb in
    if v <= 0 then
      None
    else
      Some v
  in
  let action =
    try
      Definitions.mk_run_provers ?timeout:timeout_s ?memory:memory_mb
        ~paths:req.Api.paths ~provers:req.Api.provers ~loc:None state.defs
    with Error.E e ->
      failf Error_codes.Invalid_argument "invalid job: %a" Error.pp e
  in
  let name =
    if req.Api.output_name = "" then
      Printf.sprintf "api-job-%s" (Uuidm.to_string (Misc.mk_uuid ()))
    else
      req.Api.output_name
  in
  let task =
    Task.
      {
        name;
        synopsis = None;
        action = Action.Act_run_provers action;
        defined_in = None;
      }
  in
  (* Use a ref so the on_complete closure can capture the job_id that is
     returned by push, without a recursive binding. *)
  let job_id_ref = ref "" in
  let job_id =
    Task_queue.push state.task_q task ~on_complete:(fun result_file ->
        Auth.set_job_completed state.auth ~job_id:!job_id_ref ~result_file)
  in
  job_id_ref := job_id;
  Auth.register_job state.auth ~job_id ~user_id;
  Api.make_new_job_response ~job_id ()

let handle_get_job_status state http_req (req : Api.get_job_status_request) =
  let user_id = current_user_exn http_req in
  let job_id = req.Api.job_id in
  (match Auth.get_job_user state.auth ~job_id with
  | None -> fail Error_codes.Not_found ~msg:"job not found"
  | Some uid when uid <> user_id ->
    fail Error_codes.Not_found ~msg:"job not found"
  | Some _ -> ());
  match Task_queue.job_live_status state.task_q ~uuid:job_id with
  | Task_queue.Running pct ->
    Api.make_get_job_status_response ~job_id ~status:Api.Running
      ~progress_percent:(Int32.of_int pct) ()
  | Task_queue.Queued ->
    Api.make_get_job_status_response ~job_id ~status:Api.Queued
      ~progress_percent:0l ()
  | Task_queue.Completed ->
    (* Job finished; result_file is set by the on_complete callback. *)
    let result_file =
      match Auth.get_job_completed state.auth ~job_id with
      | Some f -> f
      | None -> "" (* callback not yet flushed; treat as in-progress *)
    in
    Api.make_get_job_status_response ~job_id ~status:Api.Completed ~result_file
      ~progress_percent:100l ()
  | Task_queue.Unknown ->
    (* Not in queue and not current: check persistent state. *)
    if Auth.get_job_cancelled state.auth ~job_id then
      Api.make_get_job_status_response ~job_id ~status:Api.Cancelled
        ~progress_percent:0l ()
    else (
      match Auth.get_job_completed state.auth ~job_id with
      | Some result_file ->
        Api.make_get_job_status_response ~job_id ~status:Api.Completed
          ~result_file ~progress_percent:100l ()
      | None ->
        (* Job was registered in auth but is no longer tracked by the queue.
           This can happen after a server restart. Report as completed
           (conservative) rather than failing. *)
        Api.make_get_job_status_response ~job_id ~status:Api.Completed
          ~result_file:"" ~progress_percent:100l ()
    )

let handle_cancel_job state http_req (req : Api.cancel_job_request) =
  let user_id = current_user_exn http_req in
  let job_id = req.Api.job_id in
  (match Auth.get_job_user state.auth ~job_id with
  | None -> fail Error_codes.Not_found ~msg:"job not found"
  | Some uid when uid <> user_id ->
    fail Error_codes.Not_found ~msg:"job not found"
  | Some _ -> ());
  Auth.set_job_cancelled state.auth ~job_id;
  let _was_running : bool = Task_queue.interrupt state.task_q ~uuid:job_id in
  ()

let register ~auth ~task_q ~defs ~data_dir ~http_server =
  let state = { auth; task_q; defs; data_dir } in
  let service =
    Api.BenchpressApi.Server.make
      ~newJob:(fun rpc -> mk_handler rpc (handle_new_job state))
      ~getJobStatus:(fun rpc -> mk_handler rpc (handle_get_job_status state))
      ~cancelJob:(fun rpc -> mk_handler rpc (handle_cancel_job state))
      ()
  in
  Log.info (fun k -> k "registering API service");
  add_service
    ~middlewares:[ Auth_middleware.middleware auth ]
    http_server service

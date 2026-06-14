open Common
module H = Tiny_httpd
module U = Tiny_httpd.Util
module PB = PrintBox
module Log = (val Logs.src_log (Logs.Src.create "benchpress-serve"))

let handle_tasks (self : Server_common.t) : unit =
  let open Server_common in
  H.add_route_handler self.server ~meth:`GET H.Route.(exact "tasks" @/ return)
  @@ fun _req ->
  let@ _chrono = query_wrap (Error.wrap "serving /tasks/") in
  let tasks = Definitions.all_tasks self.defs |> List.map With_loc.view in
  let h =
    let open Html in
    let l =
      CCList.map
        (fun t ->
          let s = t.Task.name in
          mk_li []
            [
              mk_row []
                [
                  mk_col ~cls:"col-1" []
                    [
                      form
                        [
                          A.id ("launch_task" ^ s);
                          A.action ("/run/" ^ U.percent_encode s);
                          A.method_ "POST";
                        ]
                        [ mk_button ~cls:"btn-primary btn-sm" [] [ txt "run" ] ];
                    ];
                  mk_col ~cls:"col-auto" []
                    [ pre [] [ txt @@ Format.asprintf "%a@?" Task.pp t ] ];
                ];
            ])
        tasks
    in
    mk_page ~title:"tasks"
      [
        mk_navigation [ "/tasks/", "tasks", true ];
        h3 [] [ txt "list of tasks" ];
        mk_ul [] l;
      ]
  in
  H.Response.make_string ~headers:default_html_headers (Ok (Html.to_string h))

let handle_run (self : Server_common.t) : unit =
  let open Server_common in
  H.add_route_handler self.server ~meth:`POST
    ~middlewares:
      [
        Auth_middleware.middleware ~allow_localhost:self.allow_localhost
          self.auth;
      ]
    H.Route.(exact "run" @/ string_urlencoded @/ return)
  @@ fun name _r ->
  let@ _chrono = query_wrap (Error.wrapf "serving /run/%s" name) in
  Log.debug (fun k -> k "run task %S" name);
  let task =
    guardf 500 (Error.wrapf "looking for task %s" name) @@ fun () ->
    Definitions.find_task' self.defs name
  in
  Log.debug (fun k -> k "found task %s, run it" name);
  let _job_id : string = Task_queue.push self.task_q task in
  let msg =
    Format.asprintf "task queued (%d in queue)!" (Task_queue.size self.task_q)
  in
  H.Response.make_string ~headers:default_html_headers
  @@ Ok (Html.to_string @@ html_redirect ~href:"/" msg)

let handle_job_interrupt (self : Server_common.t) : unit =
  let open Server_common in
  H.add_route_handler self.server ~meth:`POST
    ~middlewares:
      [
        Auth_middleware.middleware ~allow_localhost:self.allow_localhost
          self.auth;
      ]
    H.Route.(exact "interrupt" @/ string @/ return)
  @@ fun uuid _r ->
  Log.debug (fun k -> k "interrupt current task");
  let ok = Task_queue.interrupt self.task_q ~uuid in
  if not ok then Log.err (fun k -> k "could not cancel task `%s`" uuid);
  let r = Ok (Html.to_string @@ html_redirect ~href:"/" "job interrupted") in
  H.Response.make_string ~headers:default_html_headers r

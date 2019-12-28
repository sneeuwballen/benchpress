(* run tests, or compare results *)
module T = Test
module E = CCResult

module H = Tiny_httpd
module U = Tiny_httpd_util

module Html = Tyxml_html

(* start from http://bettermotherfuckingwebsite.com/ and added some *)
let basic_css = {|
  body{margin:3rem auto;font-family: monospace;background-color:#fafafa;
  max-width:1024px;font-size:1.2rem;color:#444;padding:1rem}
  .stick{position: sticky; top: 0.5rem; background-color: lightblue; padding: 1.2rem; opacity: 90%}
  h1,h2,h3{line-height:1.2} table {width: 100%;} .framed {border-width:0.3rem; border-style: solid}
  |}

let src = Logs.Src.create "benchpress.serve"
let string_of_html h = Format.asprintf "@[%a@]@." (Html.pp ()) h

type t = {
  mutable defs: Definitions.t;
  server: H.t;
  task_q: Task_queue.t;
  data_dir: string;
}

let html_redirect (s:string) =
  let open Html in
  html
    (head (title @@ txt s)
       [style [txt basic_css];
        meta ~a:[a_http_equiv "Refresh"; a_content "0; url=/"] ()])
    (body [txt s])

(* show individual files *)
let handle_show (self:t) : unit =
  H.add_path_handler self.server ~meth:`GET "/show/%s%!" (fun file _req ->
      match Utils.load_file_summary file with
      | Error e ->
        Logs.err ~src (fun k->k "cannot load %S:\n%s" file e);
        H.Response.fail ~code:500 "could not load %S:\n%s" file e
      | Ok (file_full, cr) ->
        let box_meta = Test.Metadata.to_printbox cr.T.cr_meta in
        let box_summary = Test.Analyze.to_printbox_l cr.T.cr_analyze in
        let box_stat = Test.Stat.to_printbox_l cr.T.cr_stat in
        let bad = Test.Analyze.to_printbox_bad_l cr.T.cr_analyze in
        let errors = Test.Analyze.to_printbox_errors_l cr.T.cr_analyze in
        let box_compare_l = Test.Comparison_short.to_printbox_l cr.T.cr_comparison in
        let cactus_plot =
          let open E.Infix in
          try
            Test.Cactus_plot.of_file file_full >|= fun plot ->
            Test.Cactus_plot.to_png plot
          with e ->
            let e = Printexc.to_string e in
            Logs.err ~src (fun k->k "failure to build a cactus plot: %s" e);
            Error e
        in
        let h =
          let open Html in
          let pb_html pb = PrintBox_html.to_html pb in
          html
            (head (title (txt "show")) [style [txt basic_css]])
            (body @@ List.flatten [
                [a ~a:[a_href "/"; a_class ["stick"]] [txt "back to root"];
                 h3 [txt file]];
                [a ~a:[a_href ("/show_full/"^file)] [p [txt "show full results"]];
                 a ~a:[a_href ("/show_csv/"^file)] [p [txt "download as csv"]];
                ];
                [div [pb_html box_meta]];
                (CCList.flat_map 
                  (fun (n,p) -> [h3 [txt ("stats for " ^ n)]; div [pb_html p]])
                  box_stat);
                (CCList.flat_map 
                  (fun (n,p) -> [h3 [txt ("summary for " ^ n)]; div [pb_html p]])
                  box_summary);
                CCList.flat_map
                  (fun (n,p) ->
                     [h3 [txt ("bad for " ^ n)];
                      details ~a:[a_open()] (summary [txt "list of bad results"])
                        [div [pb_html p]]])
                  bad;
                CCList.flat_map
                  (fun (n,p) ->
                     [h3 [txt ("errors for " ^ n)];
                      details (summary [txt "list of errors"]) [div [pb_html p]]])
                  errors;
                (match cactus_plot with
                 | Error e -> [p ~a:[a_style "color: red"] [txt "could not load cactus plot"; txt e]]
                 | Ok p ->
                   Logs.debug ~src (fun k->k "encode png file of %d bytes" (String.length p));
                   [img
                      ~src:("data:image/png;base64, " ^ Base64.encode_string p)
                      ~a:[a_style "display:block; width: 100%"]
                      ~alt:"cactus plot of provers" ()]
                );
                (CCList.flat_map 
                  (fun (n1,n2,p) -> [h3 [txt (Printf.sprintf "comparison %s/%s" n1 n2)]; div [pb_html p]])
                  box_compare_l);
            ])
        in
        Logs.debug ~src (fun k->k "successful reply for %S" file);
        H.Response.make_string (Ok (string_of_html h))
    )

(* show full table for a file *)
let handle_show_full (self:t) : unit =
  H.add_path_handler self.server ~meth:`GET "/show_full/%s%!" (fun file _req ->
      match Utils.load_file file with
      | Error e ->
        Logs.err ~src (fun k->k "cannot load %S:\n%s" file e);
        H.Response.fail ~code:500 "could not load %S:\n%s" file e
      | Ok res ->
        let full_table = Test.Top_result.to_printbox_table res in
        let h =
          let open Html in
          let pb_html pb = PrintBox_html.to_html pb in
          html
            (head (title (txt "show full table")) [style [txt basic_css]])
            (body @@ List.flatten [
                [a ~a:[a_href "/"; a_class ["stick"]] [txt "back to root"]];
                [h3 [txt "full results"];
                 div [pb_html full_table]];
            ])
        in
        Logs.debug ~src (fun k->k "successful reply for %S" file);
        H.Response.make_string (Ok (string_of_html h))
    )

(* export as CSV *)
let handle_show_csv (self:t): unit =
  H.add_path_handler self.server ~meth:`GET "/show_csv/%s%!" (fun file _req ->
      match Utils.load_file file with
      | Error e ->
        Logs.err ~src (fun k->k "cannot load %S:\n%s" file e);
        H.Response.fail ~code:500 "could not load %S:\n%s" file e
      | Ok res ->
        let csv = Test.Top_result.to_csv_string res in
        Logs.debug ~src (fun k->k "successful reply for /show_csv/%S" file);
        H.Response.make_string
          ~headers:["Content-Type", "plain/csv";
                    "Content-Disposition", "attachment; filename=\"results.csv\""]
          (Ok csv)
    )

(* compare files *)
let handle_compare server : unit =
  H.add_path_handler server ~meth:`POST "/compare" (fun req ->
      let body = H.Request.body req |> String.trim in
      Logs.debug (fun k->k "/compare: body is %s" body);
      let names =
        CCString.split_on_char '&' body
        |> CCList.filter_map
          (fun s -> match CCString.Split.left ~by:"=" (String.trim s) with
             | Some (name, "on") -> Some name
             | _ -> None)
      in
      Logs.debug (fun k->k "/compare: names is [%s]" @@ String.concat ";" names);
      if List.length names>=2 then (
        let files =
          names
          |> List.map
            (fun s -> match Utils.mk_file_full s with
               | Error e ->
                 Logs.err ~src (fun k->k "cannot load file %S" s);
                 H.Response.fail_raise ~code:404 "invalid file %S: %s" s e
               | Ok x -> x)
        in
        let box_compare_l =
          let open PrintBox in
          CCList.diagonal files
          |> List.map (fun (f1,f2) ->
              let c =
                match Test_compare.Short.make f1 f2 with
                | Ok x -> x
                | Error e ->
                  Logs.err ~src (fun k->k"cannot compare %s and %s: %s" f1 f2 e);
                  H.Response.fail_raise ~code:500 "cannot compare %s and %s" f1 f2
              in
              vlist ~bars:false [
                text f1; text f2;
                Test.pb_v_record @@
                List.map (fun (pr,c) -> pr, Test_compare.Short.to_printbox c)
                  c
              ])
          |> vlist
        in
        let h =
          let open Html in
          html
            (head (title (txt "compare")) [style [txt basic_css]])
            (body [
                a ~a:[a_href "/"; a_class ["stick"]] [txt "back to root"];
                h3 [txt "comparison"];
                div [PrintBox_html.to_html box_compare_l];
              ])
        in
        H.Response.make_string (Ok (string_of_html h))
      ) else (
        H.Response.fail ~code:412 "precondition failed: select at least 2 files"
      )
    )

(* delete files *)
let handle_delete server : unit =
  H.add_path_handler server ~meth:`POST "/delete/" (fun req ->
      let body = H.Request.body req |> String.trim in
      Logs.debug (fun k->k "/delete: body is %s" body);
      let names =
        CCString.split_on_char '&' body
        |> CCList.filter_map
          (fun s -> match CCString.Split.left ~by:"=" (String.trim s) with
             | Some (name, "on") -> Some name
             | _ -> None)
      in
      Logs.debug (fun k->k "/delete: names is [%s]" @@ String.concat ";" names);
      let files =
        names
        |> List.map
          (fun s -> match Utils.mk_file_full s with
             | Error e ->
               Logs.err ~src (fun k->k "cannot load file %S" s);
               H.Response.fail_raise ~code:404 "invalid file %S: %s" s e
             | Ok x -> x)
      in
      List.iter (fun file ->
          Logs.info ~src (fun k->k  "delete file %s" @@ Filename.quote file);
          Sys.remove file)
        files;
      let h = html_redirect @@ Format.asprintf "deleted %d files" (List.length files) in
      H.Response.make_string (Ok (string_of_html h))
    )

let handle_provers (self:t) : unit =
  H.add_path_handler self.server ~meth:`GET "/provers/" (fun _r ->
      let provers = Definitions.all_provers self.defs in
      let h =
        let open Html in
        let l =
          List.map (fun p -> li [pre [txt @@ Format.asprintf "@[<v>%a@]" Prover.pp p]]) provers
        in
        html
          (head (title (txt "tasks")) [style [txt basic_css]])
          (body [
              a ~a:[a_href "/"; a_class ["stick"]] [txt "back to root"];
              h3 [txt "list of provers"];
              ul l
            ])
      in
      H.Response.make_string (Ok (string_of_html h))
    )

let handle_tasks (self:t) : unit =
  H.add_path_handler self.server ~meth:`GET "/tasks/" (fun _r ->
      let tasks = Definitions.all_tasks self.defs in
      let h =
        let open Html in
        let l =
          List.map
            (fun t ->
               let s = t.Task.name in
               li [
                 pre [txt @@Format.asprintf "%a@?" Task.pp t];
                 form ~a:[a_id (uri_of_string @@ "launch_task"^s);
                          a_action (uri_of_string @@ "/run/" ^ U.percent_encode s);
                          a_method `Post;]
                   [button ~a:[a_button_type `Submit; a_class ["stick"]]
                      [txt "run"];
                   ];
               ])
            tasks
        in
        html
          (head (title (txt "tasks")) [style [txt basic_css]])
          (body [
              a ~a:[a_href "/"; a_class ["stick"]] [txt "back to root"];
              h3 [txt "list of tasks"];
              ul l;
            ])
      in
      H.Response.make_string (Ok (string_of_html h))
    )

let handle_run (self:t) : unit =
  H.add_path_handler self.server ~meth:`POST "/run/%s" (fun name _r ->
      Logs.debug (fun k->k "run task %S" name);
      let name =
        U.percent_decode name
        |> CCOpt.get_lazy (fun () -> H.Response.fail_raise ~code:404 "cannot find task %S" name)
      in
      let task =
        match Definitions.find_task self.defs name with
        | Ok t -> t
        | Error e -> H.Response.fail_raise ~code:404 "cannot find task %s: %s" name e
      in
      Logs.debug (fun k->k "found task %s, run it" name);
      Task_queue.push self.task_q task;
      let msg =
        Format.asprintf "task queued (%d in queue)!" (Task_queue.size self.task_q)
      in
      H.Response.make_string @@ Ok (string_of_html @@ html_redirect msg)
    )

let handle_job_interrupt (self:t) : unit =
  H.add_path_handler self.server ~meth:`POST "/interrupt/" (fun _r ->
      Logs.debug (fun k->k "interrupt current task");
      let r =
        match Task_queue.cur_job self.task_q with
        | None -> Ok (string_of_html @@ html_redirect "no job to interrupt.")
        | Some j ->
          Task_queue.Job.interrupt j;
          Ok (string_of_html @@ html_redirect "job interrupted")
      in
      H.Response.make_string r
    )

(* index *)
let handle_root (self:t) : unit =
  H.add_path_handler self.server ~meth:`GET "/%!" (fun _req ->
      let entries = Utils.list_entries self.data_dir in
      let h =
        let open Html in
        html
          (head(title (txt "index")) [style [txt basic_css]])
          (body [
              ul @@ List.flatten [
                [li [a ~a:[a_href "/provers/"] [txt "provers"]];
                 li [a ~a:[a_href "/tasks/"] [txt "tasks"]]];
                (match Task_queue.cur_job self.task_q with
                 | None -> []
                 | Some j ->
                   (* display current job *)
                   [li [txt @@
                        Format.asprintf "jobs in queue: %d" (Task_queue.size self.task_q)];
                    li
                      [pre [txt @@
                             Format.asprintf "current task: %a" Task_queue.Job.pp j];
                        form ~a:[a_id (uri_of_string "cancel");
                            a_action (uri_of_string "/interrupt/");
                                a_method `Post;]
                          [button ~a:[a_button_type `Submit; a_class ["stick"]]
                             [txt "interrupt"]]];
                   ];
                )
              ];
              h3 [txt "list of results"];
              let l = 
                List.map
                  (fun (s,size) ->
                     let s = Filename.basename s in
                     let href =
                       Printf.sprintf "/show/%s" (U.percent_encode ~skip:(fun c->c='/') s)
                     in
                     li [
                       a ~a:[a_href href] [txt s];
                       txt (Printf.sprintf "(%s)" (Misc.human_size size));
                       input ~a:[a_input_type `Checkbox; a_name s] ()
                     ])
                  entries
              in
              form ~a:[a_id (uri_of_string "compare");
                       a_method `Post;]
                [button ~a:[a_button_type `Submit; a_class ["stick"]; a_formaction "/compare/"]
                   [txt "compare selected"];
                 button ~a:[a_button_type `Submit; a_class ["stick"]; a_formaction "/delete/"]
                   [txt "delete selected"];
                 ul l];
            ])
      in
      H.Response.make_string (Ok (string_of_html h))
    )

let main ?port (defs:Definitions.t) () =
  try
    let server = H.create ?port () in
    let data_dir = Misc.data_dir () in
    let self = { defs; server; data_dir; task_q=Task_queue.create ~defs (); } in
    (* thread to execute tasks *)
    let _th_r = Thread.create Task_queue.loop self.task_q in
    (* trick: see if debug level is active *)
    Logs.debug (fun k ->
      H._enable_debug true;
      k "enable http debug"
      );
    Printf.printf "listen on http://localhost:%d/\n%!" (H.port server);
    handle_root self;
    handle_show self;
    handle_show_full self;
    handle_show_csv self;
    handle_tasks self;
    handle_provers self;
    handle_run self;
    handle_job_interrupt self;
    handle_compare server;
    handle_delete server;
    H.run server |> E.map_err Printexc.to_string
  with e ->
    E.of_exn_trace e




(* run tests, or compare results *)
module T = Test
module E = CCResult

module H = Tiny_httpd
module U = Tiny_httpd_util

module Html = Tyxml_html

(* start from http://bettermotherfuckingwebsite.com/ and added some *)
let basic_css = {|
  body{margin:3rem auto;font-family: monospace;
  max-width:1024px;font-size:1.2rem;color:#444;padding:1rem}
  .stick{position: sticky; top: 0.5rem; background-color: lightblue; padding: 1.2rem; opacity: 90%}
  h1,h2,h3{line-height:1.2} table {width: 100%;} .framed {border-width:0.3rem; border-style: solid}
  |}

let src = Logs.Src.create "benchpress.serve"
let string_of_html h = Format.asprintf "@[%a@]@." (Html.pp ()) h

(* show individual files *)
let handle_show server : unit =
  H.add_path_handler server ~meth:`GET "/show/%s%!" (fun file _req ->
      match Utils.load_file file with
      | Error e ->
        Logs.err ~src (fun k->k "cannot load %S:\n%s" file e);
        H.Response.fail ~code:500 "could not load %S:\n%s" file e
      | Ok res ->
        let box_summary = Test.Top_result.to_printbox_summary res in
        let box_stat = Test.Top_result.to_printbox_stat res in
        let bad = Test.Top_result.to_printbox_bad res in
        let h =
          let open Html in
          let pb_html pb = PrintBox_html.to_html pb in
          html
            (head (title (txt "show")) [style [txt basic_css]])
            (body @@ List.flatten [
                [a ~a:[a_href "/"; a_class ["stick"]] [txt "back to root"];
                 h3 [txt file]];
                (CCList.flat_map 
                  (fun (n,p) -> [h3 [txt ("stats for " ^ n)]; div [pb_html p]])
                  box_stat);
                (CCList.flat_map 
                  (fun (n,p) -> [h3 [txt ("summary for " ^ n)]; div [pb_html p]])
                  box_summary);
                CCList.flat_map
                  (fun (n,p) -> [h3 [txt ("bad for " ^ n)]; div [pb_html p]])
                  bad;
                [a ~a:[a_href ("/show_full/"^file)] [txt "show full results"]];
            ])
        in
        Logs.debug ~src (fun k->k "successful reply for %S" file);
        H.Response.make_string (Ok (string_of_html h))
    )

(* show full table for a file *)
let handle_show_full server : unit =
  H.add_path_handler server ~meth:`GET "/show_full/%s%!" (fun file _req ->
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

(* TODO: restore this
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
            (fun s -> match Utils.load_file s with
               | Error e ->
                 Logs.err ~src (fun k->k "cannot load file %S" s);
                 H.Response.fail_raise ~code:404 "invalid file %S: %s" s e
               | Ok x -> s, x)
        in
        let box_compare_l =
          let open PrintBox in
          CCList.diagonal files
          |> List.map (fun ((f1,r1),(f2,r2)) ->
              let c = Test.Top_result.compare r1 r2 in
              vlist ~bars:false [
                text f1; text f2;
                Test.Top_result.comparison_to_printbox ~short:true c])
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
        Logs.err ~src (fun k->k "ok reply for compare %s" @@ String.concat ";" names);
        H.Response.make_string (Ok (string_of_html h))
      ) else (
        H.Response.fail ~code:412 "precondition failed: select at least 2 files"
      )
    )
   *)

let handle_provers server : unit =
  H.add_path_handler server ~meth:`GET "/provers/" (fun _r ->
      let defs = Utils.get_definitions () |> E.get_or_failwith in
      let provers = Definitions.all_provers defs in
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

let handle_tasks server : unit =
  H.add_path_handler server ~meth:`GET "/tasks/" (fun _r ->
      let defs = Utils.get_definitions () |> E.get_or_failwith in
      let tasks = Definitions.all_tasks defs in
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

let handle_run server : unit =
  H.add_path_handler server ~meth:`POST "/run/%s" (fun name _r ->
      Logs.debug (fun k->k "run task %S" name);
      let defs = Utils.get_definitions () |> E.get_or_failwith in
      let name =
        U.percent_decode name
        |> CCOpt.get_lazy (fun () -> H.Response.fail_raise ~code:404 "cannot find task %S" name)
      in
      let _task =
        match Definitions.find_task defs name with
        | Ok t -> t
        | Error e -> H.Response.fail_raise ~code:404 "cannot find task %s: %s" name e
      in
      Logs.debug (fun k->k "found task %s" name);
      H.Response.fail_raise ~code:500 "not implemented: run task"
        (* TODO: push the task into a global queue *)
    )

(* index *)
let handle_root server data_dir : unit =
  H.add_path_handler server ~meth:`GET "/%!" (fun _req ->
      let entries = Utils.list_entries data_dir in
      let h =
        let open Html in
        html
          (head(title (txt "index")) [style [txt basic_css]])
          (body [
              (* TODO: display running queue *)
              ul [
                li [a ~a:[a_href "/provers/"] [txt "provers"]];
                li [a ~a:[a_href "/tasks/"] [txt "tasks"]];
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
                       a_action (uri_of_string "/compare/");
                       a_method `Post;]
                [button ~a:[a_button_type `Submit; a_class ["stick"]]
                   [txt "compare selected"]; ul l];
            ])
      in
      H.Response.make_string (Ok (string_of_html h))
    )

let main ?port () =
  try
    let server = H.create ?port () in
    (* trick: see if debug level is active *)
    Logs.debug (fun k ->
      H._enable_debug true;
      k "enable http debug"
      );
    Printf.printf "listen on http://localhost:%d/\n%!" (H.port server);
    let data_dir = Filename.concat (Xdg.data_dir()) !Xdg.name_of_project in
    handle_root server data_dir;
    handle_show server;
    handle_show_full server;
    handle_tasks server;
    handle_provers server;
    handle_run server;
    (* FIXME:
       handle_compare server; *)
    H.run server |> E.map_err Printexc.to_string
  with e ->
    E.of_exn_trace e




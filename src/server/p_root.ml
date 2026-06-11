open Common
module H = Tiny_httpd
module U = Tiny_httpd.Util
module PB = PrintBox
module Log = (val Logs.src_log (Logs.Src.create "benchpress-serve"))

(* serve list of benchmarks *)
let handle_list_benchs (self : Server_common.t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "list-benchs" @/ return)
  @@ fun req ->
  let@ chrono = Server_common.query_wrap (Error.wrap "serving /list-benchs/") in

  let params = H.Request.query req in
  let off = try int_of_string (List.assoc "off" params) with _ -> 0 in
  let limit = try int_of_string (List.assoc "limit" params) with _ -> 20 in

  Log.debug (fun k -> k "list-benchs off=%d limit=%d" off limit);

  let html = Server_common.html_of_files self ~off ~limit in
  let resp =
    String.concat "\n" @@ List.map Server_common.Html.to_string_elt html
  in
  Log.debug (fun k ->
      k "listed %d results in %.3fs" limit (Misc.Chrono.since_last chrono));
  H.Response.make_string (Ok resp)

(* index *)
let handle_root (self : Server_common.t) : unit =
  H.add_route_handler self.server ~meth:`GET H.Route.(return) @@ fun _req ->
  let open Server_common in
  let@ chrono = query_wrap (Error.wrap "serving /") in

  let h =
    let open Html in
    mk_page ~title:"benchpress"
      [
        h1 [] [ txt "Benchpress" ];
        div
          [ A.class_ "container" ]
          [
            h2 [] [ txt "configuration" ];
            ul [ A.class_ "nav nav-tabs" ]
            @@ List.flatten
                 [
                   [
                     li
                       [ A.class_ "nav-item" ]
                       [ mk_a [ A.href "/provers/" ] [ txt "provers" ] ];
                     li
                       [ A.class_ "nav-item" ]
                       [ mk_a [ A.href "/tasks/" ] [ txt "tasks" ] ];
                     li
                       [ A.class_ "nav-item" ]
                       [ mk_a [ A.href "/compare2/ " ] [ txt "compare" ] ];
                   ];
                 ];
          ];
        (* External jobs progress bar — SSE + 30s poll fallback *)
        div
          [
            A.id "ext-jobs-status";
            "data-sse-progress", "";
            "data-sse-href", "/api/ext-jobs-status/";
            "data-sse-swap", "innerHTML";
            "hx-get", "/api/ext-jobs-status/";
            "hx-trigger", "load, every 30s";
            "hx-swap", "innerHTML";
          ]
          [];
        div
          [ A.class_ "container" ]
          [
            h2 [] [ txt "list of results" ];
            form
              [ A.id "compare"; A.method_ "POST" ]
              [
                mk_row ~cls:"m-2" []
                  [
                    mk_col ~cls:"col-auto p-1" []
                      [
                        mk_button ~cls:"btn-primary btn-sm"
                          [ A.formaction "/compare/" ]
                          [ txt "compare selected" ];
                      ];
                  ];
                (* initial list *)
                mk_ul [ A.id "list-of-res" ]
                @@ html_of_files ~off:0 ~limit:20 self;
              ];
          ];
      ]
  in
  Log.debug (fun k ->
      k "listed results in %.3fs" (Misc.Chrono.since_last chrono));
  (try Jemalloc.epoch () with _ -> ());
  H.Response.make_string ~headers:default_html_headers (Ok (Html.to_string h))

(* summary for a file. Called in lazy-load typically. *)
let handle_file_summary (self : Server_common.t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "file-sum" @/ string_urlencoded @/ return)
  @@ fun file _req ->
  let open Server_common in
  let@ chrono = query_wrap (Error.wrapf "serving /file-summary/%s" file) in
  let file_full = Bin_utils.mk_file_full file in
  let fname = Filename.basename file_full in
  let h =
    let open Html in
    try
      let m = Meta_cache.find self.meta_cache file_full in
      div [] (mk_file_summary fname m)
    with Error.E e | E (e, _) ->
      let title = [ A.title @@ "<no metadata>: " ^ Error.show e ] in
      mk_a (A.href (uri_show fname) :: title) [ txt fname ]
  in
  let r =
    H.Response.make_string ~headers:default_html_headers
      (Ok (Html.to_string_elt h))
  in
  Log.debug (fun k ->
      k "summary for %s in %.3fs" file (Misc.Chrono.since_last chrono));
  r

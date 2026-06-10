open Common
module H = Tiny_httpd
module U = Tiny_httpd.Util
module PB = PrintBox
module Log = (val Logs.src_log (Logs.Src.create "benchpress-serve"))

(* show individual files *)
let handle_show (self : Server_common.t) : unit =
  let open Server_common in
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show" @/ string_urlencoded @/ return)
  @@ fun file _req ->
  let@ chrono = query_wrap (Error.wrapf "serving %s" @@ uri_show file) in
  Log.debug (fun k -> k "----- start show %s -----" file);
  let _file_full, cr = Bin_utils.load_file_summary ~full:false file in
  Log.debug (fun k ->
      k "show: loaded summary in %.3fs" (Misc.Chrono.since_last chrono));
  let box_meta =
    (* link to the prover locally *)
    let link prover =
      PB.link (PB.text prover) ~uri:(uri_prover_in file prover)
    in
    Test_metadata.to_printbox ~link cr.cr_meta
  in
  let box_summary =
    Test_analyze.to_printbox_l
      ~link:(fun p r ->
        uri_show_detailed ~filter_prover:p ~filter_expect:r file)
      cr.cr_analyze
  in
  let box_stat =
    let to_link prover tag =
      uri_show_detailed ~filter_prover:prover ~filter_res:tag file
    in
    Test_stat.to_printbox_l ~to_link cr.cr_stat
  in
  (* TODO: make one table instead? with links to detailed comparison
     (i.e. as-table with proper filters) *)
  let box_compare_l = Test_comparison_short.to_printbox_l cr.cr_comparison in
  let uri_err = uri_error_bad file in
  let uri_invalid = uri_invalid file in
  Log.debug (fun k ->
      k "rendered to PB in %.3fs" (Misc.Chrono.since_last chrono));
  let h =
    let open Html in
    mk_page' ~title:"show"
      [
        sub_l
          [
            mk_navigation [ uri_show file, "show", true ];
            div
              [
                "hx-get", "/api/ext-jobs-status/";
                "hx-trigger", "load, every 4s";
                "hx-swap", "innerHTML";
              ]
              [];
            h3 [] [ txt file ];
            mk_row []
              (CCList.map
                 (fun x -> mk_col ~cls:"col-auto" [] [ x ])
                 [
                   mk_a ~cls:"btn-info btn-sm"
                     [ A.href (uri_show_detailed file) ]
                     [ txt "show individual results" ];
                   mk_a ~cls:"btn-info btn-sm"
                     [ A.href (uri_show_csv file) ]
                     [ txt "download as csv" ];
                   mk_a ~cls:"btn-info btn-sm"
                     [ A.href (uri_show_table file) ]
                     [ txt "show table of results" ];
                 ]);
            h3 [] [ txt "Summary" ];
            div [] [ pb_html box_meta ];
            h3 [] [ txt "User Metadata" ];
            div
              [
                "hx-get", spf "/user-meta/%s/" (U.percent_encode file);
                "hx-trigger", "load";
                "hx-swap", "innerHTML";
              ]
              [ txt "Loading..." ];
            h3 [] [ txt "stats" ];
            div [] [ pb_html box_stat ];
            h3 [] [ txt "summary" ];
            mk_a ~cls:"btn-link btn-sm h-50"
              [
                A.href (Printf.sprintf "/show_csv/%s/" (U.percent_encode file));
              ]
              [ txt "download as csv" ];
            mk_a ~cls:"btn-link btn-sm"
              [ A.href (uri_show_detailed file) ]
              [ txt "see detailed results" ];
            div [] [ pb_html box_summary ];
          ];
        sub_l
          [
            div [ A.class_ "lazy-load"; "x_src", uri_err ] [];
            div [ A.class_ "lazy-load"; "x_src", uri_invalid ] [];
            h3 [] [ txt "cactus plot" ];
            echarts_cactus_div file;
            h3 [] [ txt "evolution of similar runs" ];
            echarts_evolution_div file;
          ];
        (if box_compare_l = PB.empty then
           `Nil
         else
           sub_l
             [ h3 [] [ txt "comparisons" ]; div [] [ pb_html box_compare_l ] ]);
      ]
  in
  Log.debug (fun k ->
      k "show: turned into html in %.3fs" (Misc.Chrono.since_last chrono));
  Log.debug (fun k -> k "show: successful reply for %S" file);
  H.Response.make_string ~headers:default_html_headers (Ok (Html.to_string h))

(* prover in a given file *)
let handle_prover_in (self : Server_common.t) : unit =
  let open Server_common in
  H.add_route_handler self.server ~meth:`GET
    H.Route.(
      exact "prover-in" @/ string_urlencoded @/ string_urlencoded @/ return)
  @@ fun file p_name _req ->
  let@ _chrono = query_wrap (Error.wrapf "prover-in-file/%s/%s" file p_name) in
  Log.debug (fun k -> k "----- start prover-in %s %s -----" file p_name);
  let@ db =
    Bin_utils.with_file_as_db
      ~map_err:(Error.wrapf "reading file '%s'" file)
      file
  in
  let prover = Prover.of_db db p_name in
  let open Html in
  let h =
    mk_page ~title:"prover"
      [
        mk_navigation
          [
            uri_show file, "file", false;
            uri_prover_in file p_name, "prover", true;
          ];
        div []
          [ pre [] [ txt @@ Format.asprintf "@[<v>%a@]" Prover.pp prover ] ];
      ]
  in
  H.Response.make_string (Ok (Html.to_string h))

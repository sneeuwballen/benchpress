open Common
module H = Tiny_httpd
module U = Tiny_httpd.Util
module PB = PrintBox
module Log = (val Logs.src_log (Logs.Src.create "benchpress-serve"))

(* show full table for a file *)
let handle_show_as_table (self : Server_common.t) : unit =
  let open Server_common in
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show_table" @/ string_urlencoded @/ return)
  @@ fun file req ->
  let@ chrono = query_wrap (Error.wrapf "serving show-table/%s" file) in
  let params = H.Request.query req in
  Logs.debug (fun k ->
      k "serving /show_table/, params=%s"
        (String.concat ";"
        @@ List.map (fun (x, y) -> Printf.sprintf "%s=%s" x y) params));
  let offset =
    try List.assoc "offset" params |> int_of_string with Not_found -> 0
  in
  let filter_pb = try List.assoc "pb" params with Not_found -> "" in
  let filter_res =
    try trf_of_string @@ List.assoc "res" params with Not_found -> None
  in

  let page_size = 25 in
  let@ db =
    Bin_utils.with_file_as_db ~map_err:(Error.wrapf "using DB '%s'" file) file
  in
  let full_table =
    let link_res prover pb ~res =
      PB.link ~uri:(uri_show_single file prover pb) (PB.text res)
    in
    Test_top_result.db_to_printbox_table ?filter_res ~filter_pb ~offset
      ~link_pb:link_get_file ~page_size ~link_res db
  in
  Log.debug (fun k ->
      k "loaded table[offset=%d] in %.3fs" offset
        (Misc.Chrono.since_last chrono));
  let h =
    let open Html in
    (* pagination buttons *)
    (* FIXME: only display next if not complete *)
    let params = List.remove_assoc "offset" params in
    let btns =
      [
        mk_a
          ~cls:
            ((if offset > 0 then
                ""
              else
                "disabled ")
            ^ "page-link link-sm my-1 p-1")
          [
            A.href
              (uri_show_table ~params ~offset:(max 0 (offset - page_size)) file);
          ]
          [ txt "prev" ];
        mk_a ~cls:"page-link link-sm my-1 p-1"
          [ A.href (uri_show_table ~params ~offset:(offset + page_size) file) ]
          [ txt "next" ];
      ]
    in
    mk_page ~title:"show full table"
      [
        mk_navigation ~btns
          [
            uri_show file, "file", false;
            ( uri_show_table file,
              (if offset = 0 then
                 "full"
               else
                 spf "full[%d..]" offset),
              true );
          ];
        div
          [ A.class_ "container-fluid" ]
          [
            form
              [
                A.action (uri_show_table file);
                A.method_ "GET";
                A.class_ "form-row form-inline";
              ]
              [
                input
                  [
                    A.name "pb";
                    A.class_ "form-control form-control-sm m-3 p-3";
                    A.value filter_pb;
                    A.placeholder "problem";
                    A.type_ "text";
                  ];
                select
                  [ A.name "res"; A.class_ "form-control select m-3" ]
                  (List.map
                     (fun trf ->
                       let sel =
                         if Some trf = filter_res then
                           [ A.selected "" ]
                         else
                           []
                       in
                       let s = Test_top_result.string_of_trf trf in
                       option (sel @ [ A.value s ]) [ txt s ])
                     [ Test_top_result.TRF_all; TRF_bad; TRF_different ]);
                mk_button ~cls:"btn-info btn-sm btn-success m-3" []
                  [ txt "filter" ];
              ];
          ];
        h3 [] [ txt "full results" ];
        div [] [ pb_html full_table ];
      ]
  in
  Log.debug (fun k -> k "successful reply for %S" file);
  H.Response.make_string (Ok (Html.to_string h))

(* show list of individual results with URLs to single results for a file *)
let handle_show_detailed (self : Server_common.t) : unit =
  let open Server_common in
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show_detailed" @/ string_urlencoded @/ return)
  @@ fun db_file req ->
  let@ chrono = query_wrap (Error.wrapf "serving show_detailed/%s" db_file) in
  let params = H.Request.query req in
  let offset =
    try List.assoc "offset" params |> int_of_string with Not_found -> 0
  in
  let filter_res = try List.assoc "res" params with Not_found -> "" in
  let filter_expect =
    try expect_of_string @@ List.assoc "expect" params with Not_found -> None
  in
  let filter_prover = try List.assoc "prover" params with Not_found -> "" in
  let filter_pb = try List.assoc "pb" params with Not_found -> "" in
  let page_size = 25 in
  Log.debug (fun k ->
      k "-- show detailed file=%S offset=%d pb=`%s` res=`%s` prover=`%s` --"
        db_file offset filter_pb filter_res filter_prover);
  let@ db =
    Bin_utils.with_file_as_db
      ~map_err:(Error.wrapf "using DB '%s'" db_file)
      db_file
  in
  let l, n, complete =
    Test_detailed_res.list_keys ~page_size ~offset ~filter_prover ~filter_res
      ?filter_expect ~filter_pb db
  in
  Log.debug (fun k ->
      k "got %d results in %.3fs, complete=%B" (List.length l)
        (Misc.Chrono.elapsed chrono)
        complete);
  let open Html in
  (* pagination buttons *)
  let btns =
    [
      mk_a
        ~cls:
          ((if offset > 0 then
              ""
            else
              "disabled ")
          ^ "page-link link-sm my-1 p-1")
        [
          A.href
            (uri_show_detailed
               ~offset:(max 0 (offset - page_size))
               ~filter_res ~filter_pb ~filter_prover db_file);
        ]
        [ txt "prev" ];
      mk_a ~cls:"page-link link-sm my-1 p-1"
        [
          A.href
            (uri_show_detailed ~offset:(offset + page_size) ~filter_res
               ~filter_pb ~filter_prover db_file);
        ]
        [ txt "next" ];
    ]
  in
  mk_page ~title:"detailed results"
  @@ List.flatten
       [
         [
           mk_navigation ~btns
             [
               uri_show db_file, "file", false;
               ( uri_show_detailed db_file,
                 (if offset = 0 then
                    "detailed"
                  else
                    spf "detailed [%d..%d]" offset (offset + List.length l - 1)),
                 true );
             ];
           div
             [ A.class_ "container" ]
             [
               h2 [] [ txt (spf "detailed results (%d total)" n) ];
               div [ A.class_ "navbar navbar-expand-lg" ]
               @@ [
                    div [ A.class_ "container-fluid" ]
                    @@ [
                         form
                           [
                             A.action (uri_show_detailed db_file);
                             A.method_ "GET";
                             A.class_ "form-row form-inline";
                           ]
                           [
                             input
                               [
                                 A.name "prover";
                                 A.class_ "form-control form-control-sm m-1 p-1";
                                 A.value filter_prover;
                                 A.placeholder "prover";
                                 A.type_ "text";
                               ];
                             input
                               [
                                 A.name "pb";
                                 A.class_ "form-control form-control-sm m-1 p-1";
                                 A.value filter_pb;
                                 A.placeholder "problem";
                                 A.type_ "text";
                               ];
                             input
                               [
                                 A.name "res";
                                 A.class_ "form-control form-control-sm m-1 p-1";
                                 A.value filter_res;
                                 A.placeholder "result";
                                 A.type_ "text";
                               ];
                             input
                               [
                                 A.name "expect";
                                 A.class_ "form-control form-control-sm m-1 p-1";
                                 A.value
                                   (try List.assoc "expect" params
                                    with _ -> "");
                                 A.list "expect_l";
                               ];
                             mk_button
                               ~cls:"btn-info btn-sm btn-success m-1 p-1" []
                               [ txt "filter" ];
                           ];
                         datalist
                           [ A.id "expect_l"; A.class_ "datalist m-1" ]
                           (List.map
                              (fun v -> option [ A.value v ] [ txt v ])
                              l_all_expect);
                       ];
                  ];
             ];
           (let rows =
              CCList.map
                (fun {
                       Test_detailed_res.prover;
                       file = pb_file;
                       res;
                       file_expect;
                       rtime;
                     } ->
                  let url_file_res = uri_show_single db_file prover pb_file in
                  let url_file = uri_get_file pb_file in
                  tr []
                    [
                      td [] [ txt prover ];
                      td []
                        [
                          mk_a
                            [ A.href url_file_res; A.title pb_file ]
                            [ txt pb_file ];
                          mk_a
                            [ A.href url_file; A.title pb_file ]
                            [ txt "(content)" ];
                        ];
                      td [] [ txt (Res.to_string res) ];
                      td [] [ txt (Res.to_string file_expect) ];
                      td [] [ txt (Misc.human_duration rtime) ];
                    ])
                l
            in
            let thead =
              CCList.map
                (fun x -> th [] [ txt x ])
                [ "prover"; "file"; "res"; "expected"; "time" ]
              |> tr [] |> CCList.return |> thead []
            in
            table [ A.class_ "framed table table-striped" ] (thead :: rows));
         ];
       ]
  |> Html.to_string |> CCResult.return
  |> H.Response.make_string ~headers:default_html_headers

(* show invidual result *)
let handle_show_single (self : Server_common.t) : unit =
  let open Server_common in
  H.add_route_handler self.server ~meth:`GET
    H.Route.(
      exact "show_single" @/ string_urlencoded @/ string_urlencoded
      @/ string_urlencoded @/ return)
  @@ fun db_file prover pb_file _req ->
  let@ chrono =
    query_wrap
      (Error.wrapf "serving show_single db=%s prover=%s file=%s" db_file prover
         pb_file)
  in
  Log.debug (fun k ->
      k "show single called with prover=%s, pb_file=%s" prover pb_file);
  H.Response.make_string ~headers:default_html_headers
  @@ let@ db =
       Bin_utils.with_file_as_db
         ~map_err:(Error.wrapf "using DB '%s'" db_file)
         db_file
     in
     let r, check_res = Test_detailed_res.get_res db prover pb_file in
     let pb, pb_prover, stdout, stderr, proof_stdout =
       Test_detailed_res.to_printbox ~link:(fun _ -> link_get_file) r check_res
     in
     let open Html in
     let h =
       mk_page ~title:"single result"
         [
           mk_navigation
             [
               uri_show db_file, "file", false;
               uri_show_detailed db_file, "detailed", false;
               uri_show_single db_file prover pb_file, "single", true;
             ];
           h2 [] [ txt @@ Printf.sprintf "results for %s on %s" prover pb_file ];
           div [] [ pb_html pb ];
           details []
             [
               summary
                 [ A.class_ "alert alert-secondary" ]
                 [ txt "full stdout" ];
               pre [] [ txt stdout ];
             ];
           details []
             [
               summary
                 [ A.class_ "alert alert-secondary" ]
                 [ txt "full stderr" ];
               pre [] [ txt stderr ];
             ];
           details []
             [
               summary
                 [ A.class_ "alert alert-secondary" ]
                 [ txt "prover config" ];
               pb_html pb_prover;
             ];
           (match proof_stdout with
           | None -> div [] []
           | Some s ->
             details []
               [
                 summary
                   [ A.class_ "alert alert-secondary" ]
                   [ txt "proof checker stdout" ];
                 pre [] [ txt s ];
               ]);
         ]
     in
     Log.debug (fun k -> k "render page in %.3fs" (Misc.Chrono.elapsed chrono));
     Ok (Html.to_string h)

(* export as CSV *)
let handle_show_csv (self : Server_common.t) : unit =
  let open Server_common in
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show_csv" @/ string_urlencoded @/ return)
  @@ fun db_file req ->
  let@ chrono = query_wrap (Error.wrapf "serving show_csv/%s" db_file) in
  let@ db =
    Bin_utils.with_file_as_db
      ~map_err:(Error.wrapf "using DB '%s'" db_file)
      db_file
  in
  let query = H.Request.query req in
  Log.debug (fun k ->
      k "query: [%s]"
        (String.concat ";"
        @@ CCList.map (fun (x, y) -> Printf.sprintf "%S=%S" x y) query));
  let provers =
    try Some (List.assoc "provers" query |> CCString.split_on_char ',')
    with _ -> None
  in
  let csv = Test_top_result.db_to_csv_string ?provers db in
  Log.debug (fun k ->
      k "successful reply in %.3fs for /show_csv/%S/"
        (Misc.Chrono.elapsed chrono)
        db_file);
  H.Response.make_string
    ~headers:
      [
        "Content-Type", "plain/csv";
        "Content-Disposition", "attachment; filename=\"results.csv\"";
      ]
    (Ok csv)

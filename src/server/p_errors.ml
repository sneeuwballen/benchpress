open Common
module H = Tiny_httpd
module U = Tiny_httpd.Util
module PB = PrintBox
module Log = (val Logs.src_log (Logs.Src.create "benchpress-serve"))

let handle_show_errors (self : Server_common.t) : unit =
  let open Server_common in
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show-err" @/ string_urlencoded @/ return)
  @@ fun file _req ->
  let@ chrono = query_wrap (Error.wrapf "serving show-err/%s" file) in
  Log.debug (fun k -> k "----- start show-err %s -----" file);
  let _file_full, cr = Bin_utils.load_file_summary ~full:true file in
  Log.debug (fun k ->
      k "show-err: loaded full summary in %.3fs" (Misc.Chrono.since_last chrono));
  let link_file = link_show_single file in
  let bad = Test_analyze.to_printbox_bad_l ~link:link_file cr.cr_analyze in
  let errors =
    Test_analyze.to_printbox_errors_l ~link:link_file cr.cr_analyze
  in
  Log.debug (fun k ->
      k "rendered to PB in %.3fs" (Misc.Chrono.since_last chrono));
  let mk_dl_file l =
    let open Html in
    let data =
      "data:text/plain;base64, " ^ Base64.encode_string (String.concat "\n" l)
    in
    mk_a
      [ A.class_ "btn btn-link btn-sm"; A.download "problems.txt"; A.href data ]
      [ txt "download list" ]
  in
  let h =
    let open Html in
    (* FIXME: only optional? *)
    div' []
      [
        (*           mk_page ~title:"show-err" @@ *)
        sub_l
          (CCList.flat_map
             (fun (n, l, p) ->
               [
                 h3 [] [ txt ("bad for " ^ n) ];
                 details
                   [ A.open_ "" ]
                   [
                     summary
                       [ A.class_ "alert alert-danger" ]
                       [ txt "list of bad results" ];
                     div [] [ mk_dl_file l; pb_html p ];
                   ];
               ])
             bad);
        sub_l
          (CCList.flat_map
             (fun (n, l, p) ->
               [
                 h3 [] [ txt ("errors for " ^ n) ];
                 details []
                   [
                     summary
                       [ A.class_ "alert alert-warning" ]
                       [ txt "list of errors" ];
                     div [] [ mk_dl_file l; pb_html p ];
                   ];
               ])
             errors);
      ]
  in
  Log.debug (fun k ->
      k "show: turned into html in %.3fs" (Misc.Chrono.since_last chrono));
  Log.debug (fun k -> k "successful reply for %S" file);
  H.Response.make_string (Ok (Html.to_string_elt h))

let handle_show_invalid (self : Server_common.t) : unit =
  let open Server_common in
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show-invalid" @/ string_urlencoded @/ return)
  @@ fun file _req ->
  let@ chrono = query_wrap (Error.wrapf "serving show-invalid/%s" file) in
  Log.debug (fun k -> k "----- start show-invalid %s -----" file);
  let _file_full, cr = Bin_utils.load_file_summary ~full:true file in
  Log.debug (fun k ->
      k "show-invalid: loaded full summary in %.3fs"
        (Misc.Chrono.since_last chrono));
  let link_file = link_show_single file in
  let invalid =
    Test_analyze.to_printbox_invalid_proof_l ~link:link_file cr.cr_analyze
  in
  Log.debug (fun k ->
      k "rendered to PB in %.3fs" (Misc.Chrono.since_last chrono));
  let mk_dl_file l =
    let open Html in
    let data =
      "data:text/plain;base64, " ^ Base64.encode_string (String.concat "\n" l)
    in
    mk_a ~cls:"btn btn-link btn-sm"
      [ A.download "problems.txt"; A.href data ]
      [ txt "download list" ]
  in
  let h =
    let open Html in
    (* FIXME: only optional? *)
    div []
      ((*           mk_page ~title:"show-err" @@ *)
       CCList.flat_map
         (fun (n, l, p) ->
           [
             h3 [] [ txt ("bad for " ^ n) ];
             details
               [ A.open_ "" ]
               [
                 summary
                   [ A.class_ "alert alert-danger" ]
                   [ txt "list of invalid proofs" ];
                 div [] [ mk_dl_file l; pb_html p ];
               ];
           ])
         invalid)
  in
  Log.debug (fun k ->
      k "show-info: turned into html in %.3fs" (Misc.Chrono.since_last chrono));
  Log.debug (fun k -> k "successful reply for %S" file);
  H.Response.make_string (Ok (Html.to_string_elt h))

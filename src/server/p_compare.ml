open Common
module H = Tiny_httpd
module U = Tiny_httpd.Util
module PB = PrintBox
module Log = (val Logs.src_log (Logs.Src.create "benchpress-serve"))

(* compare different provers *)
let handle_compare2 (self : Server_common.t) : unit =
  let open Server_common in
  let server = self.server in
  H.add_route_handler server ~meth:`GET H.Route.(exact "compare2" @/ return)
  @@ fun req ->
  let@ _chrono = query_wrap (Error.wrap "serving /compare2/") in
  let provers =
    List.filter_map (fun (key, value) ->
        if key = "prover[]" then (
          match String.split_on_char '/' value with
          | [ file; prover ] -> Some (file, prover)
          | _ -> None
        ) else
          None)
    @@ H.Request.query req
  in
  let status =
    match
      String.lowercase_ascii @@ List.assoc "status" (H.Request.query req)
    with
    | "sat" -> Some `Sat
    | "unsat" -> Some `Unsat
    | _ | (exception Not_found) -> None
  in
  (* H.Request.query reverses the query order, so we have to reverse it again *)
  let provers = List.rev provers in
  let prover1, prover2 =
    match provers with
    | [ prover1; prover2 ] -> Some prover1, Some prover2
    | _ -> None, None
  in
  let entries, _more = Bin_utils.list_entries self.data_dir in
  let mk_entry ?selected _idx (file_path, _size) : Html.elt =
    let open Html in
    let file_basename = Filename.basename file_path in
    let meta = Meta_cache.find self.meta_cache file_path in
    optgroup
      [ A.label (Uuidm.to_string meta.uuid) ]
      (CCList.map
         (fun prover ->
           let attrs =
             A.value (file_basename ^ "/" ^ prover)
             ::
             (if selected = Some (file_basename, prover) then
                [ A.selected "selected" ]
              else
                [])
           in
           option attrs [ txt prover ])
         meta.provers)
  in
  let options1 = CCList.mapi (mk_entry ?selected:prover1) entries in
  let options2 = CCList.mapi (mk_entry ?selected:prover2) entries in
  let status_opt_to_string = function
    | Some `Sat -> "sat"
    | Some `Unsat -> "unsat"
    | None -> ""
  in
  let status_option ?current status =
    let open Html in
    let attrs =
      A.value (status_opt_to_string status)
      ::
      (if current = Some status then
         [ A.selected "selected" ]
       else
         [])
    in
    option attrs [ txt (status_opt_to_string status) ]
  in
  let ostatus =
    [
      status_option None;
      status_option ~current:status (Some `Unsat);
      status_option ~current:status (Some `Sat);
    ]
  in
  let prover_info =
    let file_link fname text =
      PrintBox.link ~uri:(uri_get_file fname) (PrintBox.text text)
    in
    match provers with
    | [ (f1, p1); (f2, p2) ] ->
      let ff1 = Bin_utils.mk_file_full f1 in
      let ff2 = Bin_utils.mk_file_full f2 in
      let get (short : Test_compare.Short.t) = function
        | `Improved -> short.improved
        | `Regressed -> short.regressed
        | `Mismatch -> short.mismatch
        | `Same -> short.same
        | `Solved -> short.solved
      in
      let filter_to_string = function
        | `Improved -> "Improved"
        | `Regressed -> "Regressed"
        | `Mismatch -> "Mismatch"
        | `Same -> "Same"
        | `Solved -> "Solved"
      in
      let short = Test_compare.Short.make_provers ?status (ff1, p1) (ff2, p2) in
      let make filter =
        let total = get short filter in
        let page_size = min total 500 in
        let limit_info =
          if total <= page_size then
            string_of_int total
          else
            Format.asprintf "1-%d of %d" page_size total
        in
        if total > 0 then
          [
            Html.(
              details []
                [
                  summary []
                    [ txtf "%s (%s)" (filter_to_string filter) limit_info ];
                  Test_compare.Full.make_filtered ?status ~page_size ~filter
                    (ff1, p1) (ff2, p2)
                  |> Test_compare.Full.to_printbox ~file_link
                  |> Html.pb_html;
                ]);
          ]
        else
          []
      in
      [ short |> Test_compare.Short.to_printbox |> Html.pb_html ]
      @ make `Mismatch @ make `Regressed @ make `Improved
    | _ -> []
  in
  let plot_html =
    match provers with
    | [] -> []
    | _ ->
      [
        List.map (fun (f, p) -> f ^ "/" ^ p) provers
        |> String.concat "," |> echarts_cactus_div;
      ]
  in
  let plot_html =
    if Option.is_none status then
      plot_html
    else
      Html.(
        strong []
          [ txt "Warning: this plot includes BOTH sat and unsat results." ])
      :: plot_html
  in
  let html =
    let open Html in
    mk_page ~title:"compare2"
      ([
         mk_navigation [ "/compare2/", "compare", true ];
         h3 [] [ txt "compare" ];
         form
           [ A.class_ "container" ]
           [
             div
               [ A.class_ "row" ]
               [
                 div
                   [ A.class_ "col-6" ]
                   [
                     select [ A.name "prover[]" ] options1;
                     select [ A.name "prover[]" ] options2;
                     mk_button ~cls:"btn-primary btn-sm" [] [ txt "Compare" ];
                   ];
                 div
                   [ A.class_ "col-3" ]
                   [
                     Html.label [] [ Html.txt "Limit to:" ];
                     select [ A.name "status" ] ostatus;
                   ];
               ];
           ];
       ]
      @ prover_info
      @ [ div [] plot_html ])
  in
  H.Response.make_string ~headers:default_html_headers
    (Ok (Html.to_string html))

(* compare files *)
let handle_compare (self : Server_common.t) : unit =
  let open Server_common in
  let server = self.server in
  H.add_route_handler server ~meth:`POST H.Route.(exact "compare" @/ return)
  @@ fun req ->
  let body = H.Request.body req |> String.trim in
  let@ _chrono =
    query_wrap (Error.wrapf "serving: compare (post) body=%s" body)
  in
  Log.debug (fun k -> k "/compare: body is %s" body);
  let body =
    U.parse_query body
    |> Misc.unwrap_str (fun () -> spf "parse-query failed on %s" body)
  in
  let names =
    CCList.filter_map
      (fun (k, v) ->
        if v = "on" then
          Some k
        else
          None)
      body
  in
  Log.debug (fun k -> k "/compare: names is [%s]" @@ String.concat ";" names);
  if List.length names >= 2 then (
    let files =
      names
      |> CCList.map (fun s ->
             match Bin_utils.mk_file_full s with
             | exception Error.E e ->
               Log.err (fun k -> k "cannot load file %S" s);
               H.Response.fail_raise ~code:404 "invalid file %S: %s" s
                 (Error.show e)
             | x -> x)
    in
    let box_compare_l =
      let open PrintBox in
      CCList.diagonal files
      |> CCList.map (fun (f1, f2) ->
             let c =
               match Test_compare.Short.make f1 f2 with
               | x -> x
               | exception Error.E e ->
                 Log.err (fun k ->
                     k "cannot compare %s and %s: %s" f1 f2 @@ Error.show e);
                 H.Response.fail_raise ~code:500 "cannot compare %s and %s" f1
                   f2
             in
             vlist ~bars:false
               [
                 sprintf "old: %s" f1;
                 sprintf "new: %s" f2;
                 Test.pb_v_record
                 @@ CCList.map
                      (fun (pr, c) -> pr, Test_compare.Short.to_printbox c)
                      c;
               ])
      |> vlist
    in
    let h =
      let open Html in
      mk_page ~title:"compare"
        [
          mk_navigation [];
          h3 [] [ txt "comparison" ];
          div [] [ pb_html box_compare_l ];
          echarts_cactus_div (String.concat "," names);
        ]
    in
    H.Response.make_string ~headers:default_html_headers (Ok (Html.to_string h))
  ) else
    H.Response.fail ~code:412 "precondition failed: select at least 2 files"

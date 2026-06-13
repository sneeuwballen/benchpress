open Common
module T = Test
module H = Tiny_httpd
module U = Tiny_httpd.Util
module PB = PrintBox
module Log = (val Logs.src_log (Logs.Src.create "benchpress-serve"))
module Logger = Logger
module Ext_jobs = Ext_jobs
module PB_html = PB_html
module Html = Html

type expect_filter =
  | TD_expect_improved
  | TD_expect_ok
  | TD_expect_disappoint
  | TD_expect_bad
  | TD_expect_error

type t = {
  mutable defs: Definitions.t;
  server: H.t;
  task_q: Task_queue.t;
  data_dir: string;
  meta_cache: Meta_cache.t;
  allow_delete: bool;
  allow_localhost: bool;
  auth: Auth.t;
  ext_jobs: Ext_jobs.t;
}

let html_redirect ~href (str : string) : Html.elt =
  let open Html in
  mk_page
    ~meta:[ A.http_equiv "Refresh"; A.content (spf "0; url=%s" href) ]
    ~title:str
    [ txt str ]

(* navigation bar *)
let mk_navigation ?(btns = []) path =
  let open Html in
  let path = ("/", "root", false) :: path in
  div1 [ A.class_ "sticky-top container" ]
  @@ nav'
       [ A.class_ "breadcrumb" ]
       [
         sub_e
           (ol [ A.class_ "breadcrumb navbar-header col-sm-6 m-1" ]
           @@ CCList.map
                (fun (uri, descr, active) ->
                  li
                    [
                      A.class_
                        ("breadcrumb-item "
                        ^
                        if active then
                          "active"
                        else
                          "");
                    ]
                    [ mk_a [ A.href uri ] [ txt descr ] ])
                path);
         (if btns = [] then
            `Nil
          else
            sub_e
              (div
                 [
                   A.class_
                     "btn-group-vertical col-sm-1 align-items-center \
                      navbar-right m-2";
                 ]
                 btns));
       ]

(* default reply headers *)
let default_html_headers =
  H.Headers.([] |> set "content-type" "text/html; charset=utf-8")

let uri_show file =
  Printf.sprintf "/show/%s/" (U.percent_encode ~skip:(fun c -> c = '/') file)

let uri_show_single db_file prover path =
  spf "/show_single/%s/%s/%s/" (U.percent_encode db_file)
    (U.percent_encode prover) (U.percent_encode path)

let link_show_single db_file prover path =
  PB.link (PB.text path) ~uri:(uri_show_single db_file prover path)

let uri_get_file pb = spf "/get-file/%s/" (U.percent_encode pb)
let uri_echarts pb = spf "/show-echarts/%s/" (U.percent_encode pb)
let uri_evolution pb = spf "/evolution/%s/" (U.percent_encode pb)
let uri_error_bad pb = spf "/show-err/%s/" (U.percent_encode pb)
let uri_invalid pb = spf "/show-invalid/%s/" (U.percent_encode pb)

let echarts_cactus_div ?(height = "400px") pb =
  let open Html in
  div
    [
      A.style (spf "width:100%%;height:%s" height);
      "data-chart-type", "line";
      "data-url", uri_echarts pb;
      "data-chart-loading", "true";
    ]
    []

let echarts_evolution_div ?(height = "400px") pb =
  let open Html in
  div
    [
      A.style (spf "width:100%%;height:%s" height);
      "data-chart-type", "line";
      "data-url", uri_evolution pb;
      "data-chart-loading", "true";
    ]
    []

let uri_show_detailed ?(offset = 0) ?(filter_prover = "") ?(filter_pb = "")
    ?(filter_res = "") ?(filter_expect = "") pb =
  spf "/show_detailed/%s/?%s%s%s%soffset=%d" (U.percent_encode pb)
    (if filter_prover = "" then
       ""
     else
       spf "prover=%s&" @@ U.percent_encode filter_prover)
    (if filter_pb = "" then
       ""
     else
       spf "pb=%s&" @@ U.percent_encode filter_pb)
    (if filter_res = "" then
       ""
     else
       spf "res=%s&" @@ U.percent_encode filter_res)
    (if filter_expect = "" then
       ""
     else
       spf "expect=%s&" @@ U.percent_encode filter_expect)
    offset

let uri_list_benchs ~off ?limit () : string =
  spf "/list-benchs/?%s"
    (String.concat "&"
    @@ List.flatten
         [
           [ spf "off=%d" off ];
           (match limit with
           | None -> []
           | Some l -> [ spf "limit=%d" l ]);
         ])

let enc_params ?(params = []) s =
  List.fold_left
    (fun s (k, v) ->
      Printf.sprintf "%s&%s=%s" s (U.percent_encode k) (U.percent_encode v))
    s params

let uri_prover_in file prover =
  spf "/prover-in/%s/%s/" (U.percent_encode file) (U.percent_encode prover)

let uri_show_table ?params ?(offset = 0) file =
  spf "/show_table/%s/?offset=%d" (U.percent_encode file) offset
  |> enc_params ?params

let uri_show_csv file = spf "/show_csv/%s" (U.percent_encode file)
let link_get_file pb = PB.link (PB.text pb) ~uri:(uri_get_file pb)

exception E of Error.t * int

let fail code e = raise (E (e, code))
let failf code fmt = Fmt.kasprintf (fun s -> fail code (Error.make s)) fmt

let guardf code wrap f =
  try f () with
  | Error.E err -> raise (E (wrap err, code))
  | E (err, code) -> raise (E (wrap err, code))

(* wrap the query to turn results into failed queries
   @param f takes a chrono and a [scope] for failing *)
let query_wrap wrap (f : Misc.Chrono.t -> _) : H.Response.t =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "query" in
  let chrono = Misc.Chrono.start () in
  let f' () =
    try f chrono
    with Sqlite3_utils.Type_error d ->
      failf 500 "db type error on %s" (Sqlite3_utils.Data.to_string_debug d)
  in

  match guardf 500 wrap f' with
  | h ->
    let code = h.H.Response.code in
    let succ = code >= 200 && code < 300 in
    let duration = Misc.Chrono.elapsed chrono in
    Log.debug (fun k ->
        k "%s (code %d) after %.3fs"
          (if succ then
             "successful reply"
           else
             "failure")
          code duration);
    h
  | exception E (e, code) ->
    let duration = Misc.Chrono.elapsed chrono in
    let err = wrap e in
    Log.err (fun k ->
        k "error after %.3fs (code %d):\n%a" duration code Error.pp err);
    H.Response.fail ~code "internal error after %.3fs:\n%s" duration
      (Error.show err)

let to_str_with_errcode i err = Error.show err, i
let add_errcode i err = err, i

let trace_middleware : H.Middleware.t =
 fun h req ~resp ->
  let@ _span = Trace.with_span ~parent:None ~__FILE__ ~__LINE__ "http.handle" in
  Trace.add_data_to_span _span
    [
      "http.path", `String req.path;
      "http.method", `String (H.Meth.to_string req.meth);
    ];
  let resp (response : H.Response.t) =
    let size =
      match response.body with
      | `String s -> [ "body.size", `Int (String.length s) ]
      | `Void -> [ "body.size", `Int 0 ]
      | `Stream _ | `Writer _ -> []
    in
    Trace.add_data_to_span _span
      (size @ [ "http.response.code", `Int response.code ]);
    resp response
  in
  try h req ~resp
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Opentelemetry_trace.record_exception _span exn bt;
    Printexc.raise_with_backtrace exn bt

(** user metadata: render the fragment (table + add form) *)
let render_user_meta_html file entries =
  let open Html in
  let file_enc = U.percent_encode file in
  let table_rows =
    List.map
      (fun (k, v) ->
        tr []
          [
            td [] [ txt k ];
            td [] [ txt v ];
            td []
              [
                mk_button ~cls:"btn-danger btn-sm"
                  [
                    ( "hx-delete",
                      spf "/user-meta/%s/%s/" file_enc (U.percent_encode k) );
                    "hx-target", "#user-meta";
                    "hx-swap", "innerHTML";
                    "hx-confirm", spf "Delete metadata '%s'?" k;
                  ]
                  [ txt "delete" ];
              ];
          ])
      entries
  in
  div
    [ A.id "user-meta" ]
    ((if table_rows = [] then
        [ p [ A.class_ "text-secondary" ] [ txt "No user metadata." ] ]
      else
        [
          table
            [ A.class_ "table table-sm" ]
            ([
               thead []
                 [
                   tr []
                     [ th [] [ txt "Key" ]; th [] [ txt "Value" ]; th [] [] ];
                 ];
             ]
            @ [ tbody [] table_rows ]);
        ])
    @ [
        form
          [
            A.class_ "row g-2 mt-2";
            "hx-post", spf "/user-meta/%s/" file_enc;
            "hx-target", "#user-meta";
            "hx-swap", "innerHTML";
          ]
          [
            div
              [ A.class_ "col-auto" ]
              [
                input
                  [
                    A.name "key";
                    A.type_ "text";
                    A.placeholder "key";
                    A.class_ "form-control form-control-sm";
                    A.required "";
                  ];
              ];
            div
              [ A.class_ "col-auto" ]
              [
                input
                  [
                    A.name "value";
                    A.type_ "text";
                    A.placeholder "value";
                    A.class_ "form-control form-control-sm";
                    A.required "";
                  ];
              ];
            div
              [ A.class_ "col-auto" ]
              [ mk_button ~cls:"btn-primary btn-sm" [] [ txt "Add" ] ];
          ];
      ])

let with_result_db_read file ~f =
  let file_full = Bin_utils.mk_file_full file in
  if Bin_utils.is_sqlite_file file_full then
    Db.with_db ~timeout:1500 ~mode:`READONLY file_full f
  else if Misc.is_zst_file file_full then
    Bin_utils.with_decompressed_zst file_full (fun tmp ->
        Db.with_db ~timeout:1500 ~mode:`READONLY tmp f)
  else
    failf 400 "unsupported file format for '%s'" file

let with_result_db_write file ~f =
  let file_full = Bin_utils.mk_file_full file in
  if Bin_utils.is_sqlite_file file_full then
    Db.with_db ~timeout:1500 file_full f
  else
    failf 400 "writing metadata is only supported for .sqlite files, not '%s'"
      file

type prover_summary = {
  prover: string;
  sat: int;
  unsat: int;
  error: int;
  bad: int;
}
(** per-prover summary from a result DB *)

let per_prover_summary (db : Db.t) : prover_summary list =
  let count_res ~prover res_cond =
    let query =
      "select count(*) from prover_res where prover=? and " ^ res_cond ^ ";"
    in
    Db.exec db query prover
      ~ty:Db.Ty.(p1 text, p1 int, id)
      ~f:(fun c ->
        Db.Cursor.next c |> Error.unwrap_opt' (fun () -> "expected count"))
    |> Misc.unwrap_db (fun () -> spf "count for %s" prover)
  in
  let provers =
    Db.exec_no_params_exn db "select distinct name from prover;"
      ~ty:Db.Ty.(p1 any_str, id)
      ~f:Db.Cursor.to_list_rev
  in
  List.map
    (fun prover ->
      let sat = count_res ~prover "res='sat'" in
      let unsat = count_res ~prover "res='unsat'" in
      let error = count_res ~prover "res='error'" in
      let bad =
        count_res ~prover
          "res in ('sat','unsat') and res != file_expect and file_expect in \
           ('sat','unsat')"
      in
      { prover; sat; unsat; error; bad })
    provers

let trf_of_string = function
  | "bad" -> Some Test_top_result.TRF_bad
  | "different" -> Some Test_top_result.TRF_different
  | "all" -> Some Test_top_result.TRF_all
  | s ->
    Log.warn (fun k -> k "unknown table filter: %S" s);
    None

(* html for the summary of [file] with metadata [m] *)
let mk_file_summary filename (m : Test_metadata.t) : Html.elt list =
  let open Html in
  let add_title =
    let title = [ A.title (Test_metadata.to_string m) ] in
    let url_show = uri_show filename in
    fun x -> mk_a (A.href url_show :: title) [ x ]
  in

  let nres =
    let hd = add_title @@ txt (spf "%d res" m.n_results)
    and tl =
      if m.n_bad > 0 then
        [ span [ A.class_ "badge bg-danger" ] [ txt (spf "%d bad" m.n_bad) ] ]
      else
        []
    in
    span [ A.class_ "col-md-3" ] (hd :: tl)
  and provers =
    span
      [ A.class_ "col-md-3" ]
      [ txt (spf "{%s}" @@ String.concat "," m.provers) ]
  and date =
    span
      [ A.class_ "col-md-3 text-secondary" ]
      [
        txt
        @@ CCOpt.map_or ~default:"<unknown date>" Misc.human_datetime
             m.timestamp;
      ]
  and dirs =
    if CCList.is_empty m.dirs then
      []
    else (
      let title = String.concat "\n" m.dirs in
      [
        span
          [ A.title title ]
          [
            txt @@ spf "dirs {%s}" @@ String.concat ","
            @@ List.map (Misc.truncate_left 10) m.dirs;
          ];
      ]
    )
  in

  let fields = List.flatten [ [ nres; provers; date ]; dirs ] in
  fields

let l_all_expect = [ "improved"; "ok"; "disappoint"; "bad"; "error" ]

let expect_of_string s =
  match String.trim s with
  | "improved" -> Some Test_detailed_res.TD_expect_improved
  | "ok" -> Some Test_detailed_res.TD_expect_ok
  | "disappoint" -> Some Test_detailed_res.TD_expect_disappoint
  | "bad" -> Some Test_detailed_res.TD_expect_bad
  | "error" -> Some Test_detailed_res.TD_expect_error
  | "" -> None
  | e -> Error.failf "unknown 'expect' filter: %S" e

let html_of_files (self : t) ~off ~limit : Html.elt list =
  let entries, more = Bin_utils.list_entries self.data_dir ~off ~limit in

  let mk_entry idx (file_path, size) : Html.elt =
    let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "html-of-files.mk-entry" in
    Trace.add_data_to_span _sp
      [ "file_path", `String file_path; "size", `Int size ];

    let open Html in
    let file_basename = Filename.basename file_path in
    let meta = Meta_cache.find_if_loaded self.meta_cache file_path in
    let url_show = uri_show file_basename in
    let url_meta =
      Printf.sprintf "/file-sum/%s" (U.percent_encode file_basename)
    in
    let id = spf "file_%d" (off + idx) in

    let descr =
      match meta with
      | Some m ->
        let uuid_str = Uuidm.to_string m.Test_metadata.uuid in
        let is_active =
          Option.is_some (Ext_jobs.find_opt self.ext_jobs uuid_str)
        in
        if is_active then (
          let url_meta =
            Printf.sprintf "/file-sum/%s" (U.percent_encode file_basename)
          in
          let trigger =
            spf "job-update-%s from:body throttle:5s, every 300s" uuid_str
          in
          [
            div
              [
                "hx-get", url_meta;
                "hx-trigger", trigger;
                "hx-swap", "outerHTML";
              ]
              (mk_file_summary file_basename m);
          ]
        ) else
          mk_file_summary file_basename m
      | None ->
        [
          div
            [ "hx-get", url_meta; "hx-swap", "outerHTML"; "hx-trigger", "load" ]
            [ mk_a [ A.href url_show ] [ txt file_path ] ];
        ]
    in
    let a =
      if idx + 1 = limit && more = `More then
        [
          "hx-trigger", "revealed";
          "hx-swap", "afterend";
          "hx-get", uri_list_benchs ~off:(off + limit) ();
        ]
      else
        []
    in
    let a = A.class_ "list-group-item" :: A.id id :: a in

    li a
      [
        div
          [ A.class_ "row row-cols-12" ]
          [
            div [ A.class_ "col-md-7 justify-self-left" ] descr;
            h4
              [ A.class_ "col-md-2 justify-self-right" ]
              [
                span
                  [ A.class_ "badge text-secondary" ]
                  [ txt (Printf.sprintf "(%s)" (Misc.human_size size)) ];
              ];
            (if self.allow_delete then
               div
                 [ A.class_ "col-md-2 justify-self-right" ]
                 [
                   mk_button ~cls:"btn-warning btn-sm"
                     [
                       ( "hx-delete",
                         "/delete1/" ^ U.percent_encode file_path ^ "/" );
                       "hx-confirm", "Confirm deletion?";
                       "hx-target", spf "#%s" id;
                       "hx-swap", "outerHTML";
                       A.title "delete file";
                     ]
                     [ txt "delete" ];
                 ]
             else
               div [] []);
            div
              [ A.class_ "col-md-1 justify-self-right" ]
              [ input [ A.type_ "checkbox"; A.name file_basename ] ];
          ];
      ]
  in
  CCList.mapi mk_entry entries

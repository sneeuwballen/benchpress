(* run tests, or compare results *)
module T = Test
module E = CCResult

module H = Tiny_httpd
module U = Tiny_httpd_util
module PB = PrintBox

module Log = (val Logs.src_log (Logs.Src.create "benchpress-serve"))
let spf = Printf.sprintf

let[@inline] (let@@) f1 f2 = f1 @@ f2

type t = {
  mutable defs: Definitions.t;
  server: H.t;
  task_q: Task_queue.t;
  data_dir: string;
  meta_cache: (string, Test.metadata) Hashtbl.t;
  dyn_status: bool;
  allow_delete: bool;
}

(** {2 printbox -> html} *)
module PB_html : sig
  open Tyxml
  type 'a html = 'a Html.elt
  val style : [>`Style] html
  val to_html : PrintBox.t -> [`Div] html
end = struct
  open Tyxml

  module B = PrintBox
  module H = Html

  type 'a html = 'a Html.elt

  let style =
    let l = [
      "table.framed { border: 2px solid black; }";
      "table.framed th, table.framed td { border: 1px solid black; }";
      "th, td { padding: 3px; }";
    ] in
    H.style (CCList.map H.txt l)

  let attrs_of_style (s:B.Style.t) : _ list * _ =
    let open B.Style in
    let {bold;bg_color;fg_color} = s in
    let encode_color = function
      | Red -> "red"
      | Blue -> "blue"
      | Green -> "green"
      | Yellow -> "yellow"
      | Cyan -> "cyan"
      | Black -> "black"
      | Magenta -> "magenta"
      | White -> "white"
    in
    let s =
      (match bg_color with None -> [] | Some c -> ["background-color", encode_color c]) @
      (match fg_color with None -> [] | Some c -> ["color", encode_color c])
    in
    let a = match s with
      | [] -> []
      | s -> [H.a_style @@ String.concat ";" @@ CCList.map (fun (k,v) -> k ^ ": " ^ v) s] in
    a, bold

  let rec to_html_rec (b: B.t) : [< Html_types.flow5 > `Div `Ul `Table `P] html =
    match B.view b with
    | B.Empty -> H.div []
    | B.Text {l; style} ->
      let a, bold = attrs_of_style style in
      let l = CCList.map H.txt l in
      let l = if bold then CCList.map (fun x->H.b [x]) l else l in
      H.div ~a l
    | B.Pad (_, b)
    | B.Frame b -> to_html_rec b
    | B.Align {h=`Right;inner=b;v=_} ->
      H.div ~a:[H.a_class ["align-right"]] [ to_html_rec b ]
    | B.Align {h=`Center;inner=b;v=_} ->
      H.div ~a:[H.a_class ["center"]] [ to_html_rec b ]
    | B.Align {inner=b;_} -> to_html_rec b
    | B.Grid (bars, a) ->
      let class_ = match bars with
        | `Bars -> ["table-bordered"; "framed"]
        | `None -> ["table-borderless"]
      in
      let to_row a =
        Array.to_list a
        |> CCList.map (fun b -> H.td ~a:[H.a_class ["thead"]] [to_html_rec b])
        |> (fun x -> H.tr x)
      in
      let rows =
        Array.to_list a |> CCList.map to_row
      in
      H.table ~a:[H.a_class ("table" :: "table-hover" ::
                             "table-striped" :: class_)] rows
    | B.Tree (_, b, l) ->
      let l = Array.to_list l in
      H.div
        [ to_html_rec b
        ; H.ul (CCList.map (fun x -> H.li [to_html_rec x]) l)
        ]
    | B.Link {uri; inner} ->
      H.div [H.a ~a:[H.a_class ["btn-link"]; H.a_href uri] [to_html_rec inner]]
    | _ ->
      (* catch-all to be more resilient to newer versions of printbox *)
      H.div [H.pre [H.txt @@ PrintBox_text.to_string b]] (* remaining cases *)
      [@@warning "-11"]

  let to_html b = H.div [to_html_rec b]
end

module Html = struct
  include Tyxml_html

  let b_style = link ~rel:[`Stylesheet] ~href:"/css/" ()

  let mk_page ?meta:(my_meta=[]) ~title:s my_body =
    html
      (head (title @@ txt s) [
          b_style;
          PB_html.style;
          link ~rel:[`Icon] ~href:"/favicon.png" ();
          meta ~a:(a_charset "utf-8" :: my_meta) ();
          script ~a:[a_src "/js"; Unsafe.string_attrib "type" "module"] (txt "");
        ])
      (body [
          div ~a:[a_class ["container"]] my_body
        ])

  let div1 ?a x = div ?a [x]
  let mk_a ?(cls=["btn-link"]) ?a:(al=[]) x = a ~a:(a_class ("btn" :: cls) :: al) x
  let mk_row ?(cls=[]) x = div ~a:[a_class ("row"::cls)] x
  let mk_col ?(a=[]) ?(cls=[]) x = div ~a:(a_class ("col"::cls) :: a) x
  let mk_li x = li ~a:[a_class ["list-group-item"]] x
  let mk_ul l = ul ~a:[a_class ["list-group"]] l
  let mk_button ?(a=[]) ?(cls=[]) x =
    button ~a:(a_button_type `Submit :: a_class ("btn" :: cls) :: a) x

  let pb_html pb =
    div ~a:[a_class ["table"]] [PB_html.to_html pb]

  let to_string h = Format.asprintf "@[%a@]@." (pp ()) h
  let to_string_elt h = Format.asprintf "@[%a@]@." (pp_elt ()) h
end

let dyn_status self =
  let open Html in
  if self.dyn_status then (
    ul ~a:[a_id "dyn-status"; a_class ["list-group"]] []
  ) else (
    div[] (* nothing *)
  )

let html_redirect ~href (s:string) =
  let open Html in
  mk_page
    ~meta:[a_http_equiv "Refresh"; a_content (spf "0; url=%s" href)]
    ~title:s
    [txt s]

(* navigation bar *)
let mk_navigation ?(btns=[]) path =
  let open Html in
  let path = ("/", "root", false) :: path in
  div1 ~a:[a_class ["sticky-top"; "container"]] @@
  nav ~a:[a_class ["navbar"; "navbar-expand-md"]] @@
    List.flatten [
      [ol ~a:[a_class ["breadcrumb"; "navbar-header"; "col-sm-6"; "m-1"]] @@
       CCList.map (fun (uri, descr, active) ->
           li ~a:[a_class ("breadcrumb-item" :: if active then ["active"] else [])] [
             mk_a ~a:[a_href uri] [txt descr]
           ])
         path;
      ];
      (if btns=[] then []
       else [div ~a:[a_class ["btn-group-vertical"; "col-sm-1"; "align-items-center";
                              "navbar-right"; "m-2"]]
               btns]);
  ]

(* default reply headers *)
let default_html_headers = H.Headers.([] |> set "content-type" "text/html; charset=utf-8")

let uri_show file =
  Printf.sprintf "/show/%s" (U.percent_encode ~skip:(fun c->c='/') file)

let uri_show_single db_file prover path =
  spf "/show_single/%s/%s/%s/"
    (U.percent_encode db_file)
    (U.percent_encode prover)
    (U.percent_encode path)

let link_show_single db_file prover path =
  PB.link (PB.text path) ~uri:(uri_show_single db_file prover path)

let uri_get_file pb = spf "/get-file/%s" (U.percent_encode pb)
let uri_gnuplot pb = spf "/show-gp/%s" (U.percent_encode pb)
let uri_error_bad pb = spf "/show-err/%s" (U.percent_encode pb)
let uri_show_detailed
    ?(offset=0) ?(filter_prover="") ?(filter_pb="")
    ?(filter_res="") ?(filter_expect="") pb =
  spf "/show_detailed/%s/?%s%s%s%soffset=%d"
    (U.percent_encode pb)
    (if filter_prover="" then "" else spf "prover=%s&" @@ U.percent_encode filter_prover)
    (if filter_pb="" then "" else spf "pb=%s&" @@ U.percent_encode filter_pb)
    (if filter_res="" then "" else spf "res=%s&" @@ U.percent_encode filter_res)
    (if filter_expect="" then ""
     else spf "expect=%s&" @@ U.percent_encode filter_expect)
    offset

let uri_prover_in file prover =
  spf "/prover-in/%s/%s/" (U.percent_encode file) (U.percent_encode prover)
let uri_show_table ?(offset=0) file =
  spf "/show_table/%s/?offset=%d" (U.percent_encode file) offset
let uri_show_csv file = "/show_csv/"^U.percent_encode file

let link_get_file pb = PB.link (PB.text pb) ~uri:(uri_get_file pb)

(* wrap the query to turn results into failed queries
   @param f takes a chrono and a [scope] for failing *)
let query_wrap mkctx
    (f:(Misc.Chrono.t * (string*int) Misc.try_scope) -> _) =
  let chrono = Misc.Chrono.start () in
  let f' scope =
    try f (chrono, scope)
    with Sqlite3_utils.Type_error d ->
      scope.unwrap @@
      Error (spf "db type error on %s" (Sqlite3_utils.Data.to_string_debug d), 500)
  in
  Misc.err_with f'
    ~map_err:(fun (e,code) -> spf "in %s:\n%s" (mkctx()) e, code)
  |> E.catch
    ~ok:(fun h ->
        let code = h.H.Response.code in
        let succ = code >= 200 && code < 300 in
        let duration = Misc.Chrono.elapsed chrono in
        Log.debug
          (fun k->k
              "%s (code %d) after %.3fs for %s"
              (if succ then "successful reply" else "failure") code duration
              (mkctx())
            );
        h)
    ~err:(fun (e,code) ->
        let duration = Misc.Chrono.elapsed chrono in
        Log.err (fun k->k "error after %.3fs in %s (code %d):\n%s"
                    duration (mkctx()) code e);
        H.Response.fail ~code "internal error for %s:\n%s" (mkctx()) e)

let add_err_code i x = x,i

(* show individual files *)
let handle_show (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show" @/ string_urlencoded @/ return)
  @@ fun file _req ->
  let@@ chrono, scope = query_wrap (fun()-> spf "/show/%s" file) in
  Log.info (fun k->k "----- start show %s -----" file);
  let _file_full, cr =
    Bin_utils.load_file_summary ~full:false file
    |> scope.unwrap_with (add_err_code 404) in
  Log.info (fun k->k "show: loaded summary in %.3fs" (Misc.Chrono.since_last chrono));
  let box_meta =
    (* link to the prover locally *)
    let link prover = PB.link (PB.text prover) ~uri:(uri_prover_in file prover) in
    Test.Metadata.to_printbox ~link cr.T.cr_meta
  in
  let box_summary =
    Test.Analyze.to_printbox_l
      ~link:(fun p r ->
          uri_show_detailed ~filter_prover:p ~filter_expect:r file)
      cr.T.cr_analyze in
  let box_stat =
    let to_link prover tag =
      uri_show_detailed ~filter_prover:prover ~filter_res:tag file
    in
    Test.Stat.to_printbox_l ~to_link cr.T.cr_stat
  in
  (* TODO: make one table instead? with links to detailed comparison
     (i.e. as-table with proper filters) *)
  let box_compare_l =
    Test.Comparison_short.to_printbox_l cr.T.cr_comparison in
  let uri_plot = uri_gnuplot file in
  let uri_err = uri_error_bad file in
  Log.info (fun k->k "rendered to PB in %.3fs" (Misc.Chrono.since_last chrono));
  let h =
    let open Html in
    mk_page ~title:"show" @@
    List.flatten [
      [mk_navigation [uri_show file, "show", true];
       dyn_status self;
       h3 [txt file];
       mk_row @@
       CCList.map (fun x -> mk_col ~cls:["col-auto"] [x]) @@
       List.flatten [
         [ mk_a ~cls:["btn-info";"btn-sm"]
             ~a:[a_href (uri_show_detailed file)]
             [txt "show individual results"];
           mk_a ~cls:["btn-info";"btn-sm"]
             ~a:[a_href (uri_show_csv file)]
             [txt "download as csv"];
           mk_a ~cls:["btn-info";"btn-sm"]
             ~a:[a_href (uri_show_table file)]
             [txt "show table of results"];
         ];
         if self.allow_delete then [
           form ~a:[a_method `Post] [
             mk_button ~cls:["btn-danger";"btn-sm"]
               ~a:[a_formaction ("/delete1/" ^ U.percent_encode file ^ "/"); ]
               [txt "delete"];
           ]] else [];
       ]
      ];
      [h3 [txt "Summary"]; div [pb_html box_meta]];
      [h3 [txt "stats"]; div [pb_html box_stat]];
      [
        h3 [txt "summary"];
        mk_a ~cls:["btn-link"; "btn-sm"; "h-50"]
          ~a:[a_href (Printf.sprintf "/show_csv/%s"
                        (U.percent_encode file))]
          [txt "download as csv"];
        mk_a ~cls:["btn-link"; "btn-sm"]
          ~a:[a_href (uri_show_detailed file)]
          [txt "see detailed results"];
        div [pb_html box_summary];
      ];
      (* lazy loading *)
      [div ~a:[a_class ["lazy-load"];
               Unsafe.string_attrib "x_src" uri_err ] []];
      [img
         ~src:uri_plot
         ~a:[a_class ["img-fluid"];
             Unsafe.string_attrib "loading" "lazy";
            ]
         ~alt:"cactus plot of provers" ()];
      (if box_compare_l=PB.empty then []
       else [
         h3 [txt "comparisons"];
         div [pb_html box_compare_l];
       ]);
    ]
  in
  Log.info (fun k->k "show: turned into html in %.3fs"
               (Misc.Chrono.since_last chrono));
  Log.debug (fun k->k "show: successful reply for %S" file);
  H.Response.make_string ~headers:default_html_headers (Ok (Html.to_string h))

(* prover in a given file *)
let handle_prover_in (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "prover-in" @/ string_urlencoded @/ string_urlencoded @/ return)
  @@ fun file p_name _req ->
  let@@ _chrono, scope = query_wrap (fun() -> spf "prover-in-file/%s/%s" file p_name) in
  Log.info (fun k->k "----- start prover-in %s %s -----" file p_name);
  scope.unwrap @@
  let@@ scope, db = Bin_utils.with_file_as_db file in
  let prover =
    Prover.of_db db p_name |> scope.unwrap_with (add_err_code 500) in
  let open Html in
  let h = mk_page ~title:"prover"
      [
        mk_navigation [
          uri_show file, "file", false;
          uri_prover_in file p_name, "prover", true;
        ];
        dyn_status self;
        div [
          pre [txt @@ Format.asprintf "@[<v>%a@]" Prover.pp prover];
          begin match prover.Prover.defined_in with
            | None -> span[]
            | Some f ->
              div [txt "defined in"; mk_a ~a:[a_href (uri_get_file f)] [txt f]]
          end;
        ]
      ]
  in
  H.Response.make_string (Ok (Html.to_string h))

(* gnuplot for a file *)
let handle_show_gp (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show-gp" @/ string_urlencoded @/ return)
  @@ fun file _req ->
  let@@ chrono, scope = query_wrap (fun() -> spf "show-gp/%s" file) in
  Log.info (fun k->k "----- start show-gp %s -----" file);
  let file_full =
    Bin_utils.mk_file_full file
    |> scope.unwrap_with (add_err_code 404) in
  let cactus_plot =
    let open E.Infix in
    try
      Test.Cactus_plot.of_file file_full >|= fun plot ->
      Test.Cactus_plot.to_png plot
    with e ->
      let e = Printexc.to_string e in
      Log.err (fun k->k "failure to build a cactus plot: %s" e);
      Error e
  in
  Log.info (fun k->k "rendered to gplot in %.3fs" (Misc.Chrono.since_last chrono));
  let plot = cactus_plot |> scope.unwrap_with (add_err_code 500) in
  Log.debug (fun k->k "encode png file of %d bytes" (String.length plot));
  Log.debug (fun k->k "successful reply for show-gp/%S" file);
  H.Response.make_string
    ~headers:H.Headers.([] |> set "content-type" "image/png")
    (Ok plot)

let handle_show_errors (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show-err" @/ string_urlencoded @/ return)
  @@ fun file _req ->
  let@@ chrono, scope = query_wrap (fun () -> spf "show-err/%s" file) in
  Log.info (fun k->k "----- start show-err %s -----" file);
  let _file_full, cr =
    Bin_utils.load_file_summary ~full:true file
    |> scope.unwrap_with (add_err_code 404) in
  Log.info (fun k->k "show-err: loaded full summary in %.3fs"
               (Misc.Chrono.since_last chrono));
  let link_file = link_show_single file in
  let bad = Test.Analyze.to_printbox_bad_l ~link:link_file cr.T.cr_analyze in
  let errors = Test.Analyze.to_printbox_errors_l ~link:link_file cr.T.cr_analyze in
  Log.info (fun k->k "rendered to PB in %.3fs" (Misc.Chrono.since_last chrono));
  let mk_dl_file l =
    let open Html in
    let data =
      "data:text/plain;base64, "^Base64.encode_string (String.concat "\n" l)
    in
    mk_a ~cls:["btn"; "btn-link";"btn-sm"]
      ~a:[a_download (Some "problems.txt"); a_href data]
      [txt "download list"]
  in
  let h =
    let open Html in
    (* FIXME: only optional? *)
    div @@
    (*           mk_page ~title:"show-err" @@ *)
    List.flatten [
      CCList.flat_map
        (fun (n,l,p) ->
           [h3 [txt ("bad for " ^ n)];
            details ~a:[a_open()]
              (summary ~a:[a_class ["alert";"alert-danger"]]
                 [txt "list of bad results"])
              [div [mk_dl_file l; pb_html p]]])
        bad;
      CCList.flat_map
        (fun (n,l,p) ->
           [h3 [txt ("errors for " ^ n)];
            details (summary ~a:[a_class ["alert"; "alert-warning"]]
                       [txt "list of errors"; ])
              [div [mk_dl_file l; pb_html p]]])
        errors;
    ]
  in
  Log.info (fun k->k "show:turned into html in %.3fs"
               (Misc.Chrono.since_last chrono));
  Log.debug (fun k->k "successful reply for %S" file);
  H.Response.make_string (Ok (Html.to_string_elt h))

let trf_of_string = function
  | "bad" -> Some Test.TRF_bad
  | "different" -> Some Test.TRF_different
  | "all" -> Some Test.TRF_all
  | s ->
    Log.warn (fun k->k "unknown table filter: %S" s);
    None

let string_of_trf = function
  | Test.TRF_bad -> "bad"
  | Test.TRF_different -> "different"
  | Test.TRF_all -> "all"

(* show full table for a file *)
let handle_show_as_table (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show_table" @/ string_urlencoded @/ return)
  @@ fun file req ->
  let@@ chrono, scope = query_wrap (fun() -> spf "show-table/%s" file) in
  let params = H.Request.query req in
  let offset = try List.assoc "offset" params |> int_of_string with Not_found -> 0 in
  let filter_pb = try List.assoc "pb" params with Not_found -> "" in
  let filter_res =
    try trf_of_string @@ List.assoc "res" params
    with Not_found -> None
  in
  let page_size = 25 in
  scope.unwrap @@
  let@@ (_scope, db) = Bin_utils.with_file_as_db file in
  let full_table =
    let link_res prover pb ~res =
      PB.link ~uri:(uri_show_single file prover pb) (PB.text res)
    in
    Test.Top_result.db_to_printbox_table
      ?filter_res ~filter_pb ~offset ~link_pb:link_get_file
      ~page_size ~link_res db
  in
  Log.info (fun k->k "loaded table[offset=%d] in %.3fs"
               offset (Misc.Chrono.since_last chrono));
  let h =
    let open Html in
    (* pagination buttons *)
    (* FIXME: only display next if not complete *)
    let btns = [
      mk_a
        ~cls:((if offset>0 then [] else ["disabled"]) @
              ["page-link";"link-sm";"my-1"; "p-1"])
        ~a:[a_href
              (uri_show_table ~offset:(max 0 (offset-page_size)) file)]
        [txt "prev"];
      mk_a ~cls:["page-link";"link-sm"; "my-1"; "p-1"]
        ~a:[a_href
              (uri_show_table ~offset:(offset+page_size) file)]
        [txt "next"];
    ]
    in
    mk_page ~title:"show full table" @@
    List.flatten [
      [mk_navigation ~btns [
          uri_show file, "file", false;
          uri_show_table file,
          (if offset=0 then "full" else spf "full[%d..]" offset),
          true;
        ];
       dyn_status self];
      [div ~a:[a_class ["container-fluid"]] @@
       [form ~a:[a_action (uri_show_table file);
                 a_method `Get;
                 a_class ["form-row"; "form-inline"]]
          [
            input ~a:[a_name "pb";
                      a_class ["form-control"; "form-control-sm"; "m-3"; "p-3"];
                      a_value filter_pb;
                      a_placeholder "problem";
                      a_input_type `Text] ();
            select ~a:[a_name "res";
                       a_class ["form-control"; "select"; "m-3"];
                      ] @@
            List.map
              (fun trf ->
                 let sel = if Some trf = filter_res then [a_selected()] else [] in
                 let s = string_of_trf trf in
                 option ~a:(sel @[a_value s]) (txt s))
              [Test.TRF_all; Test.TRF_bad; Test.TRF_different];
            mk_button
              ~cls:["btn-info"; "btn-sm"; "btn-success"; "m-3";]
              [txt "filter"];
          ];
       ]
      ];
      [h3 [txt "full results"];
       div [pb_html full_table]];
    ]
  in
  Log.debug (fun k->k "successful reply for %S" file);
  H.Response.make_string (Ok (Html.to_string h))

(* html for the summary of [file] with metadata [m] *)
let mk_file_summary filename m : _ Html.elt =
  let open Html in
  let entry_descr, title =
    let descr = Printf.sprintf "%d res for {%s} at %s"
        m.Test.n_results (String.concat "," m.Test.provers)
        (CCOpt.map_or ~default:"<unknown date>"
           Misc.human_datetime m.Test.timestamp)
    in
    descr, [a_title (Test.Metadata.to_string m)]
  in
  let url_show =
    Printf.sprintf "/show/%s" (U.percent_encode ~skip:(fun c->c='/') filename)
  in
  mk_a ~a:(a_href url_show :: title) [txt entry_descr]

let l_all_expect = ["improved"; "ok"; "disappoint"; "bad"; "error"]

let expect_of_string s =
  match String.trim s with
  | "improved" -> Ok (Some Test.TD_expect_improved)
  | "ok" -> Ok (Some Test.TD_expect_ok)
  | "disappoint" -> Ok (Some Test.TD_expect_disappoint)
  | "bad" -> Ok (Some Test.TD_expect_bad)
  | "error" -> Ok (Some Test.TD_expect_error)
  | "" -> Ok None
  | e -> Error (spf "unknown 'expect' filter: %S" e)

(* show list of individual results with URLs to single results for a file *)
let handle_show_detailed (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show_detailed" @/ string_urlencoded @/ return)
  @@ fun db_file req ->
  let@@ chrono, scope = query_wrap (fun() -> spf "show_detailed/%s" db_file) in
  let params = H.Request.query req in
  let offset = try List.assoc "offset" params |> int_of_string with Not_found -> 0 in
  let filter_res = try List.assoc "res" params with Not_found -> "" in
  let filter_expect =
    try scope.unwrap_with (add_err_code 400) @@
      expect_of_string @@ List.assoc "expect" params
    with Not_found -> None
  in
  let filter_prover = try List.assoc "prover" params with Not_found -> "" in
  let filter_pb = try List.assoc "pb" params with Not_found -> "" in
  let page_size = 25 in
  Log.debug (fun k->k "-- show detailed file=%S offset=%d pb=`%s` res=`%s` prover=`%s` --"
                db_file offset filter_pb filter_res filter_prover);
  scope.unwrap @@
  let@@ scope, db = Bin_utils.with_file_as_db db_file in
  let l, n, complete =
    Test.Detailed_res.list_keys
      ~page_size ~offset ~filter_prover ~filter_res ?filter_expect ~filter_pb db
    |> scope.unwrap_with (add_err_code 500) in
  Log.debug (fun k->k "got %d results in %.3fs, complete=%B"
                (List.length l) (Misc.Chrono.elapsed chrono) complete);
  let open Html in
  (* pagination buttons *)
  let btns = [
    mk_a
      ~cls:((if offset>0 then [] else ["disabled"]) @
            ["page-link";"link-sm";"my-1";"p-1"])
      ~a:[a_href
            (uri_show_detailed ~offset:(max 0 (offset-page_size))
               ~filter_res ~filter_pb ~filter_prover db_file)]
      [txt "prev"];
    mk_a ~cls:["page-link";"link-sm"; "my-1"; "p-1"]
      ~a:[a_href
            (uri_show_detailed ~offset:(offset+page_size)
               ~filter_res ~filter_pb ~filter_prover db_file)]
      [txt "next"];
  ]
  in
  mk_page ~title:"detailed results" @@ List.flatten [
    [mk_navigation ~btns [
        uri_show db_file, "file", false;
        uri_show_detailed db_file,
        (if offset=0 then "detailed" else spf "detailed [%d..%d]" offset (offset+List.length l-1)),
        true;
      ];
     dyn_status self;
     div ~a:[a_class ["container"]] [
       h2 [txt (spf "detailed results (%d total)" n)];
       div ~a:[a_class ["navbar"; "navbar-expand-lg"]] @@
       [div ~a:[a_class ["container-fluid"]] @@
        [form ~a:[a_action (uri_show_detailed db_file);
                  a_method `Get;
                  a_class ["form-row"; "form-inline"]]
           [
             input ~a:[a_name "prover";
                       a_class ["form-control"; "form-control-sm"; "m-1"; "p-1"];
                       a_value filter_prover;
                       a_placeholder "prover";
                       a_input_type `Text] ();
             input ~a:[a_name "pb";
                       a_class ["form-control"; "form-control-sm"; "m-1"; "p-1"];
                       a_value filter_pb;
                       a_placeholder "problem";
                       a_input_type `Text] ();
             input ~a:[a_name "res";
                       a_class ["form-control"; "form-control-sm"; "m-1"; "p-1"];
                       a_value filter_res;
                       a_placeholder "result";
                       a_input_type `Text] ();
             input ~a:[a_name "expect";
                       a_class ["form-control-sm"; "m-1"; "p-1"];
                       a_value (try List.assoc "expect" params with _ -> "");
                       a_list "expect_l"] ();
             mk_button
               ~cls:["btn-info"; "btn-sm"; "btn-success";
                     "m-1"; "p-1"]
               [txt "filter"];
           ];
         datalist ~a:[a_id "expect_l"; a_class ["datalist"; "m-1"]; ]
           ~children:(`Options (
               List.map (fun v -> option ~a:[a_value v] @@ txt v) l_all_expect))
          ();
        ];
       ]];
     let rows =
       CCList.map
         (fun {Test.Detailed_res.prover;file=pb_file;res;file_expect;rtime} ->
            let url_file_res = uri_show_single db_file prover pb_file in
            let url_file = uri_get_file pb_file in
            tr [
              td [txt prover];
              td [
                mk_a ~a:[a_href url_file_res; a_title pb_file] [txt pb_file];
                mk_a ~a:[a_href url_file; a_title pb_file] [txt "(content)"];
              ];
              td [txt (Res.to_string res)];
              td [txt (Res.to_string file_expect)];
              td [txt (Misc.human_duration rtime)]
            ])
         l
     in
     let thead =
       CCList.map (fun x->th [txt x])
         ["prover"; "file"; "res"; "expected"; "time"]
       |> tr |> CCList.return |> thead
     in
     table ~a:[a_class ["framed"; "table"; "table-striped"]] ~thead rows;
    ];
  ]
  |> Html.to_string
  |> E.return
  |> H.Response.make_string ~headers:default_html_headers

(* show invidual result *)
let handle_show_single (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show_single" @/ string_urlencoded @/
             string_urlencoded @/ string_urlencoded @/ return)
  @@ fun db_file prover pb_file _req ->
  let@@ chrono, scope =
    query_wrap (fun() -> spf "show_single db=%s prover=%s file=%s"
                   db_file prover pb_file)
  in
  Log.debug (fun k->k "show single called with prover=%s, pb_file=%s" prover pb_file);
  H.Response.make_string ~headers:default_html_headers
  @@ scope.unwrap @@
  let@@ scope, db = Bin_utils.with_file_as_db db_file in
  let r = Test.Detailed_res.get_res db prover pb_file
          |> scope.unwrap_with (add_err_code 500) in
  let pb, pb_prover, stdout, stderr =
    Test.Detailed_res.to_printbox ~link:(fun _ -> link_get_file) r
  in
  let open Html in
  let h = mk_page ~title:"single result" [
    mk_navigation [
      uri_show db_file, "file", false;
      uri_show_detailed db_file, "detailed", false;
      uri_show_single db_file prover pb_file, "single", true;
    ];
    dyn_status self;
    h2 [txt @@ Printf.sprintf "results for %s on %s" prover pb_file];
    div [pb_html pb];
    details (summary ~a:[a_class ["alert";"alert-secondary"]] [txt "full stdout"])
      [pre [txt stdout]];
    details (summary ~a:[a_class ["alert";"alert-secondary"]] [txt "full stderr"])
      [pre [txt stderr]];
    details (summary ~a:[a_class ["alert";"alert-secondary"]] [txt "prover config"])
      [pb_html pb_prover];
  ] in
  Log.debug (fun k->k"render page in %.3fs" (Misc.Chrono.elapsed chrono));
  Ok (Html.to_string h)

(* export as CSV *)
let handle_show_csv (self:t): unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show_csv" @/ string_urlencoded @/ return)
  @@ fun db_file req ->
  let@@ chrono, scope = query_wrap (fun() -> spf "show_csv/%s" db_file) in
  scope.unwrap @@
  let@@ scope, db = Bin_utils.with_file_as_db db_file in
  let query = H.Request.query req in
  Log.debug
    (fun k->k  "query: [%s]"
        (String.concat ";"  @@
         CCList.map (fun (x,y) -> Printf.sprintf "%S=%S" x y) query));
  let provers =
    try Some (List.assoc "provers" query |> CCString.split_on_char ',')
    with _ -> None
  in
  let csv =
    try Test.Top_result.db_to_csv_string ?provers db
    with e -> scope.unwrap_with (fun e->Printexc.to_string e,500) (Error e)
  in
  Log.debug (fun k->k "successful reply in %.3fs for /show_csv/%S"
                (Misc.Chrono.elapsed chrono) db_file);
  H.Response.make_string
    ~headers:["Content-Type", "plain/csv";
              "Content-Disposition", "attachment; filename=\"results.csv\""]
    (Ok csv)

(* compare files *)
let handle_compare self : unit =
  let server = self.server in
  H.add_route_handler server ~meth:`POST
    H.Route.(exact "compare" @/ return)
  @@ fun req ->
  let body = H.Request.body req |> String.trim in
  let@@ _chrono, scope = query_wrap (fun() -> spf "compare (post) body=%s" body) in
  Log.debug (fun k->k "/compare: body is %s" body);
  let body = U.parse_query body |> scope.unwrap_with (add_err_code 400) in
  let names =
    CCList.filter_map
      (fun (k,v) -> if v="on" then Some k else None)
      body
  in
  Log.debug (fun k->k "/compare: names is [%s]" @@ String.concat ";" names);
  if List.length names>=2 then (
    let files =
      names
      |> CCList.map
        (fun s -> match Bin_utils.mk_file_full s with
           | Error e ->
             Log.err (fun k->k "cannot load file %S" s);
             H.Response.fail_raise ~code:404 "invalid file %S: %s" s e
           | Ok x -> x)
    in
    let box_compare_l =
      let open PrintBox in
      CCList.diagonal files
      |> CCList.map (fun (f1,f2) ->
          let c =
            match Test_compare.Short.make f1 f2 with
            | Ok x -> x
            | Error e ->
              Log.err (fun k->k"cannot compare %s and %s: %s" f1 f2 e);
              H.Response.fail_raise ~code:500 "cannot compare %s and %s" f1 f2
          in
          (* TODO: graph *)
          vlist ~bars:false [
            text f1; text f2;
            Test.pb_v_record @@
            CCList.map (fun (pr,c) -> pr, Test_compare.Short.to_printbox c)
              c
          ])
      |> vlist
    in
    let h =
      let open Html in
      mk_page ~title:"compare"
        [
          mk_navigation [];
          dyn_status self;
          h3 [txt "comparison"];
          div [pb_html box_compare_l];
        ]
    in
    H.Response.make_string ~headers:default_html_headers (Ok (Html.to_string h))
  ) else (
    H.Response.fail ~code:412 "precondition failed: select at least 2 files"
  )

(* delete files *)
let handle_delete self : unit =
  assert self.allow_delete;
  let run names =
    Log.debug (fun k->k "/delete: names is [%s]" @@ String.concat ";" names);
    let files =
      names
      |> CCList.map
        (fun s -> match Bin_utils.mk_file_full s with
           | Error e ->
             Log.err (fun k->k "cannot load file %S" s);
             H.Response.fail_raise ~code:404 "invalid file %S: %s" s e
           | Ok x -> x)
    in
    List.iter (fun file ->
        Log.info (fun k->k  "delete file %s" @@ Filename.quote file);
        Sys.remove file)
      files;
    let h = html_redirect ~href:"/" @@ spf "deleted %d files" (List.length files) in
    H.Response.make_string ~headers:default_html_headers (Ok (Html.to_string h))
  in
  H.add_route_handler self.server ~meth:`POST
    H.Route.(exact "delete1" @/ string_urlencoded @/ return)
    (fun file _req ->
      Log.debug (fun k->k "/delete1: path is %s" file);
      run [file]
    );
  H.add_route_handler self.server ~meth:`POST
    H.Route.(exact "delete" @/ return)
    (fun req -> let body = H.Request.body req |> String.trim in
      Log.debug (fun k->k "/delete: body is %s" body);
      let names =
        CCString.split_on_char '&' body
        |> CCList.filter_map
          (fun s -> match CCString.Split.left ~by:"=" (String.trim s) with
             | Some (name, "on") -> Some name
             | _ -> None)
      in
      Log.debug (fun k->k "/delete: names is [%s]" @@ String.concat ";" names);
      run names
    );
  ()

let handle_provers (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "provers" @/ return)
  @@ fun req ->
  let@@ _chrono, _scope = query_wrap (fun() -> "provers") in
  let name =
    try Some (List.assoc "name" @@ H.Request.query req)
    with _ -> None
  in
  let provers = match name with
    | Some name ->
      begin match Definitions.find_prover self.defs name with
        | Ok p -> [p]
        | Error e -> H.Response.fail_raise ~code:404 "no such prover: %s" e
      end
    | None -> Definitions.all_provers self.defs
  in
  let h =
    let open Html in
    let mk_prover p =
      mk_li [
        pre [txt @@ Format.asprintf "@[<v>%a@]" Prover.pp p];
        begin match p.Prover.defined_in with
          | None -> span[]
          | Some f ->
            div [txt "defined in"; mk_a ~a:[a_href (uri_get_file f)] [txt f]]
        end;
      ]
    in
    let l = CCList.map mk_prover provers in
    mk_page ~title:"provers"
      [
        mk_navigation ["/provers/", "provers", true];
        dyn_status self;
        h3 [txt "list of provers"];
        mk_ul l
      ]
  in
  H.Response.make_string ~headers:default_html_headers (Ok (Html.to_string h))

let handle_tasks (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "tasks" @/ return)
  @@ fun _req ->
  let@@ _chrono, _scopes = query_wrap (fun()->"tasks") in
  let tasks = Definitions.all_tasks self.defs in
  let h =
    let open Html in
    let l =
      CCList.map
        (fun t ->
           let s = t.Task.name in
           mk_li [
             mk_row [
               mk_col ~cls:["col-1"] [
                 form ~a:[a_id (uri_of_string @@ "launch_task"^s);
                          a_action (uri_of_string @@ "/run/" ^ U.percent_encode s);
                          a_method `Post;]
                   [mk_button ~cls:["btn-primary";"btn-sm"] [txt "run"]];
               ];
               mk_col ~cls:["col-auto"] [pre [txt @@Format.asprintf "%a@?" Task.pp t]];
               begin match t.Task.defined_in with
                 | None -> span[]
                 | Some f ->
                   div [txt "defined in"; mk_a ~a:[a_href (uri_get_file f)] [txt f]]
               end;
             ]])
        tasks
    in
    mk_page ~title:"tasks"
      [
        mk_navigation ["/tasks/", "tasks", true];
        dyn_status self;
        h3 [txt "list of tasks"];
        mk_ul l;
      ]
  in
  H.Response.make_string ~headers:default_html_headers (Ok (Html.to_string h))

let handle_run (self:t) : unit =
  H.add_route_handler self.server ~meth:`POST
    H.Route.(exact "run" @/ string_urlencoded @/ return)
  @@ fun name _r ->
  let@@ _chrono, scope = query_wrap (fun()->spf "run/%s" name) in
  Log.debug (fun k->k "run task %S" name);
  let task =
    match Definitions.find_task self.defs name with
    | Ok t -> t
    | Error e ->
      scope.unwrap (Error (spf "cannot find task %s: %s" name e, 404))
  in
  Log.debug (fun k->k "found task %s, run it" name);
  Task_queue.push self.task_q task;
  let msg =
    Format.asprintf "task queued (%d in queue)!" (Task_queue.size self.task_q)
  in
  H.Response.make_string ~headers:default_html_headers @@
  Ok (Html.to_string @@ html_redirect ~href:"/" msg)

let handle_job_interrupt (self:t) : unit =
  H.add_route_handler self.server ~meth:`POST
    H.Route.(exact "interrupt" @/ string @/ return)
  @@ fun uuid _r ->
  Log.debug (fun k->k "interrupt current task");
  let ok = Task_queue.interrupt self.task_q ~uuid in
  if not ok then Log.err (fun k->k"could not cancel task `%s`" uuid);
  let r =
    Ok (Html.to_string @@ html_redirect ~href:"/" "job interrupted")
  in
  H.Response.make_string ~headers:default_html_headers r

(* get metadata for the file *)
let get_meta (self:t) (p:string) : _ result =
  match Hashtbl.find self.meta_cache p with
  | m -> Ok m
  | exception Not_found ->
    let res =
      try
        Sqlite3_utils.with_db ~cache:`PRIVATE ~mode:`READONLY p
          (fun db -> Test.Metadata.of_db db)
      with e ->
        Error (Printf.sprintf "not a valid db: %S: %s"
                 (Filename.basename p) @@ Printexc.to_string e)
    in
    E.iter
      (fun m ->
         (* cache if it's complete *)
         if Test.Metadata.is_complete m then (
           Hashtbl.add self.meta_cache p m
         )) res;
    res

(* index *)
let handle_root (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(return)
  @@ fun _req ->
  let@@ chrono, _scope = query_wrap (fun()->"handle /") in
  let entries = Bin_utils.list_entries self.data_dir in
  let h =
    let open Html in
    mk_page ~title:"benchpress"
      [
        h1 [txt "Benchpress"];
        dyn_status self;
        div ~a:[a_class ["container"]] [
          h2 [txt "configuration"];
          ul ~a:[a_class ["list-group"]] @@ List.flatten [
            [li ~a:[a_class ["list-group-item"]]
               [mk_a ~a:[a_href "/provers/"] [txt "provers"]];
             li ~a:[a_class ["list-group-item"]]
               [mk_a ~a:[a_href "/tasks/"] [txt "tasks"]]];
          ];
        ];
        let l =
          CCList.map
            (fun (s0,size) ->
               let s = Filename.basename s0 in
               let meta = CCHashtbl.get self.meta_cache s0 in
               let url_show =
                 Printf.sprintf "/show/%s" (U.percent_encode ~skip:(fun c->c='/') s)
               and url_meta =
                 Printf.sprintf "/file-sum/%s" (U.percent_encode s)
               in
               li ~a:[a_class ["list-group-item"]]
                 [div ~a:[a_class ["row"]]
                    [
                      (* lazy loading of status *)
                      div ~a:[a_class ["col-md-9"; "justify-self-left"]]
                        (match meta with
                         | Some meta ->
                           (* metadata cached, just display it *)
                           [mk_file_summary s meta]
                         | None ->
                           (* lazy loading *)
                           [mk_a ~cls:["disabled"; "lazy-load"]
                              ~a:[a_href url_show;
                                  Unsafe.string_attrib "x_src" url_meta]
                              [txt s0]]
                        );
                      h4 ~a:[a_class ["col-md-2"]] [
                        span ~a:[a_class ["badge"; "text-secondary"]]
                          [txt (Printf.sprintf "(%s)" (Misc.human_size size))];
                      ];
                      div ~a:[a_class ["col-1"]]
                        [input ~a:[a_input_type `Checkbox; a_name s] ()];
                    ]])
            entries
        in
        Log.info (fun k->k "listed results in %.3fs" (Misc.Chrono.since_last chrono));
        div ~a:[a_class ["container"]] [
          h2 [txt "list of results"];
          form ~a:[a_id (uri_of_string "compare"); a_method `Post;] [
            mk_row ~cls:["m-2"] @@
            List.flatten [
              [mk_col ~cls:["col-auto";"p-1"] [
                  mk_button ~cls:["btn-primary";"btn-sm"]
                    ~a:[a_formaction "/compare/"]
                    [txt "compare selected"]];
              ];
              if self.allow_delete then [
                mk_col ~cls:["col-auto";"p-1"] [
                  mk_button ~cls:["btn-danger";"btn-sm"]
                    ~a:[a_formaction "/delete/"]
                    [txt "delete selected"]]
              ]
              else [];
            ];
            mk_ul l
          ]
        ];
      ]
  in
  Jemalloc.epoch();
  H.Response.make_string ~headers:default_html_headers (Ok (Html.to_string h))

let handle_file_summary (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "file-sum" @/ string_urlencoded @/ return)
  @@ fun file _req ->
  let@@ _chrono, scope = query_wrap (fun()->spf "file-summary/%s" file) in
  let file_full =
    Bin_utils.mk_file_full file |> scope.unwrap_with (add_err_code 404)
  in
  let s = Filename.basename file_full in
  let h =
    let open Html in
    get_meta self file_full
    |> E.catch
      ~err:(fun e ->
          let title = [a_title @@ "<no metadata>: "  ^ e] in
          mk_a ~a:(a_href (uri_show s) :: title) [txt s]
        )
      ~ok:(fun m -> mk_file_summary s m)
  in
  H.Response.make_string ~headers:default_html_headers (Ok (Html.to_string_elt h))

let handle_css self : unit =
  let mk_path p' ctype value =
    H.add_route_handler self ~meth:`GET H.Route.(exact p' @/ return)
      (fun req ->
        let h = Digest.to_hex (Digest.string value) in
        if H.Request.get_header req "If-None-Match" = Some h then (
           Log.debug (fun k->k "cached object (etag: %S)" h);
           H.Response.make_raw ~code:304 ""
         ) else (
          H.Response.make_string
            ~headers:["content-type", ctype; "Etag", h]
            (Ok value)
        )
      )
  in
  mk_path "css" "text/css" Web_data.css;
  mk_path "js" "text/javascript" Web_data.js;
  mk_path "favicon.png" "media/png" Web_data.favicon;
  ()

let handle_file self : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "get-file" @/ string_urlencoded @/ return)
    (fun file _req ->
      Log.debug (fun k->k "get-file: `%s`" file);
      let bytes =
        if file = "prelude" then (
          H.Byte_stream.of_string Builtin_config.config (* magic file! *)
        ) else (
          try H.Byte_stream.of_chan @@ open_in file
          with e ->
            H.Response.fail_raise ~code:404
              "cannot open file %S:\n%s\n\nThe benchmark might not be present on this machine." file
              (Printexc.to_string e)
        )
      in
      H.Response.make_raw_stream
        ~headers:["Content-Type", "text/plain"]
        ~code:200 bytes
    )

(** {2 Embedded web server} *)

module Cmd = struct
  let main
      ?(local_only=false) ?port ~dyn_status ~allow_delete
      (defs:Definitions.t) () =
    try
      let addr = if local_only then "127.0.0.1" else "0.0.0.0" in
      let server = H.create ~max_connections:32 ~addr ?port () in
      let data_dir = Misc.data_dir () in
      let self = {
        defs; server; data_dir; task_q=Task_queue.create ~defs ();
        meta_cache=Hashtbl.create ~random:true 16;
        dyn_status; allow_delete;
      } in
      (* thread to execute tasks *)
      let _th_r = Thread.create Task_queue.loop self.task_q in
      (* trick: see if debug level is active *)
      Log.debug (fun k ->
          H._enable_debug true;
          k "enable http debug"
        );
      (* maybe serve the API *)
      Printf.printf "listen on http://localhost:%d/\n%!" (H.port server);
      handle_root self;
      handle_file_summary self;
      handle_css server;
      handle_show self;
      handle_show_gp self;
      handle_prover_in self;
      handle_show_errors self;
      handle_show_as_table self;
      handle_show_detailed self;
      handle_show_single self;
      handle_show_csv self;
      handle_tasks self;
      handle_provers self;
      handle_run self;
      handle_job_interrupt self;
      handle_compare self;
      if allow_delete then handle_delete self;
      handle_file self;
      H.run server |> E.map_err Printexc.to_string
    with e ->
      E.of_exn_trace e

  (* sub-command to serve the web UI *)
  let cmd =
    let open Cmdliner in
    let port =
      Arg.(value & opt (some int) None & info ["p";"port"] ~doc:"port to listen on")
    and local_only =
      Arg.(value & flag & info ["local-only"] ~doc:"only listen on localhost")
    and dyn_status =
      Arg.(value & opt bool false & info ["dyn-status"] ~doc:"dynamic status in page")
    and allow_delete =
      Arg.(value & opt bool false & info ["allow-delete"] ~doc:"allow deletion of files")
    and defs =
      Bin_utils.definitions_term
    in
    let doc = "serve embedded web UI on given port" in
    let aux defs port local_only dyn_status allow_delete () =
      main ?port ~local_only ~dyn_status ~allow_delete defs () in
    Term.(pure aux $ defs $ port $ local_only $ dyn_status $ allow_delete $ pure () ),
    Term.info ~doc "serve"
end

let () =
  CCFormat.set_color_default true;
  match Cmdliner.Term.eval Cmd.cmd with
  | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok (Ok ()) | `Version | `Help -> ()
  | `Ok (Error e) ->
    print_endline ("error: " ^ e);
    exit 1

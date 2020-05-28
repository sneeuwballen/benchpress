(* run tests, or compare results *)
module T = Test
module E = CCResult

module H = Tiny_httpd
module U = Tiny_httpd_util
module PB = PrintBox

module Log = (val Logs.src_log (Logs.Src.create "benchpress-serve"))
let spf = Printf.sprintf

type t = {
  mutable defs: Definitions.t;
  server: H.t;
  task_q: Task_queue.t;
  data_dir: string;
  meta_cache: (string, Test.metadata) Hashtbl.t;
  dyn_status: bool;
  allow_delete: bool;
}

(* TODO: use printbox 0.5 and use custom classes with its tyxml printer *)
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
      H.table ~a:[H.a_class ("table" :: (*"table-striped" ::*) "table-hover" :: class_)] rows
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
          meta ~a:(a_charset "utf-8" :: my_meta) ();
          script ~a:[a_src "/js"; Unsafe.string_attrib "type" "module"] (txt "");
        ])
      (body [
          div ~a:[a_class ["container"]] my_body
        ])

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

let uri_show file =
  Printf.sprintf "/show/%s" (U.percent_encode ~skip:(fun c->c='/') file)

let uri_show_single db_file prover path =
  spf "/show_single/%s/%s/%s/"
    (U.percent_encode db_file)
    (U.percent_encode prover)
    (U.percent_encode path)

let link_show_single db_file prover path =
  PB.link (PB.text path) ~uri:(uri_show_single db_file prover path)

(* navigation bar *)
let mk_navigation ?(btns=[]) path =
  let open Html in
  let path = ("/", "root", false) :: path in
  div ~a:[a_class ["sticky-top"; "row"; "align-items-center"]] @@ List.flatten [
    [ol ~a:[a_class ["breadcrumb"; "col-8"]] @@
     CCList.map (fun (uri, descr, active) ->
         li ~a:[a_class ("breadcrumb-item" :: if active then ["active"] else [])] [
           mk_a ~a:[a_href uri] [txt descr]
         ])
       path];
    (if btns=[] then [] else [div ~a:[a_class ["btn-group-vertical"]] btns]);
  ]

let uri_get_file pb = spf "/get-file/%s" (U.percent_encode pb)
let uri_gnuplot pb = spf "/show-gp/%s" (U.percent_encode pb)
let uri_error_bad pb = spf "/show-err/%s" (U.percent_encode pb)
let uri_show_detailed
    ?(offset=0) ?(filter_prover="") ?(filter_pb="") ?(filter_res="") pb =
  spf "/show_detailed/%s/?%s%s%soffset=%d"
    (U.percent_encode pb)
    (if filter_prover="" then "" else spf "prover=%s&" @@ U.percent_encode filter_prover)
    (if filter_pb="" then "" else spf "pb=%s&" @@ U.percent_encode filter_pb)
    (if filter_res="" then "" else spf "res=%s&" @@ U.percent_encode filter_res)
    offset
let uri_prover_in file prover =
  spf "/prover-in/%s/%s/" (U.percent_encode file) (U.percent_encode prover)
let uri_show_table ?(offset=0) file =
  spf "/show_table/%s/?offset=%d" (U.percent_encode file) offset
let uri_show_csv file = "/show_csv/"^U.percent_encode file

let link_get_file pb = PB.link (PB.text pb) ~uri:(uri_get_file pb)

(* show individual files *)
let handle_show (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show" @/ string_urlencoded @/ return)
    (fun file _req ->
      Log.info (fun k->k "----- start show %s -----" file);
      let chrono = Misc.Chrono.start () in
      match Bin_utils.load_file_summary ~full:false file with
      | Error e ->
        Log.err (fun k->k "cannot load %S:\n%s" file e);
        H.Response.fail ~code:500 "could not load %S:\n%s" file e
      | Ok (_file_full, cr) ->
        Log.info (fun k->k "show: loaded summary in %.3fs" (Misc.Chrono.since_last chrono));
        let box_meta =
          let link prover =
            (* link to the prover locally *)
            PB.link (PB.text prover) ~uri:(uri_prover_in file prover)
          in
          Test.Metadata.to_printbox ~link cr.T.cr_meta
        in
        let box_summary = Test.Analyze.to_printbox_l cr.T.cr_analyze in
        let box_stat =
          let to_link prover tag =
            uri_show_detailed ~filter_prover:prover ~filter_res:tag file
          in
          Test.Stat.to_printbox_l ~to_link cr.T.cr_stat
        in
        let box_compare_l = Test.Comparison_short.to_printbox_l cr.T.cr_comparison in
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
            (CCList.flat_map
               (fun (n,p) -> [h3 [txt ("stats for " ^ n)]; div [pb_html p]])
               box_stat);
            (* TODO: use lazy load for this? *)
            (CCList.flat_map
               (fun (n,pb) ->
                  [h3 [txt ("summary for " ^ n)];
                   mk_a ~cls:["btn-link"; "btn-sm"; "h-50"]
                     ~a:[a_href (Printf.sprintf "/show_csv/%s?provers=%s"
                                   (U.percent_encode file) (U.percent_encode n))]
                     [txt "download as csv"];
                   mk_a ~cls:["btn-link"; "btn-sm"]
                     ~a:[a_href (uri_show_detailed ~filter_prover:n file)]
                     [txt "see detailed results"];
                   div [pb_html pb];
                  ])
               box_summary);
            (* lazy loading *)
            [div ~a:[a_class ["lazy-load"];
                     Unsafe.string_attrib "x_src" uri_err ] []];
            [img
               ~src:uri_plot
               ~a:[a_class ["img-fluid"];
                   Unsafe.string_attrib "loading" "lazy";
                  ]
               ~alt:"cactus plot of provers" ()];
            (CCList.flat_map
               (fun (n1,n2,p) ->
                  [h3 [txt (Printf.sprintf "comparison %s/%s" n1 n2)];
                   div [pb_html p]])
               box_compare_l);
          ]
        in
        Log.info (fun k->k "show:turned into html in %.3fs"
                           (Misc.Chrono.since_last chrono));
        Log.debug (fun k->k "successful reply for %S" file);
        H.Response.make_string (Ok (Html.to_string h))
    )

(* prover in a given file *)
let handle_prover_in (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "prover-in" @/ string_urlencoded @/ string_urlencoded @/ return)
    (fun file p_name _req ->
      Log.info (fun k->k "----- start prover-in %s %s -----" file p_name);
      let r = Bin_utils.with_file_as_db file (fun scope db ->
          let prover = Prover.of_db db p_name |> scope.unwrap in
          let open Html in
          mk_page ~title:"prover"
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
        ) in
      match r with
      | Ok h ->
        Log.debug (fun k->k "successful reply for prover-in/%S/%s" file p_name);
        H.Response.make_string (Ok (Html.to_string h))
      | Error e ->
        Log.err (fun k->k "error in prover-in/%S/%s:\n%s" file p_name e);
        H.Response.fail ~code:500 "could not show prover %s for %S:\n%s" file p_name e
    )

(* gnuplot for a file *)
let handle_show_gp (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show-gp" @/ string_urlencoded @/ return)
    (fun file _req ->
      Log.info (fun k->k "----- start show-gp %s -----" file);
      match Bin_utils.mk_file_full file with
      | Error e ->
        Log.err (fun k->k "cannot load %S:\n%s" file e);
        H.Response.fail ~code:500 "could not load %S:\n%s" file e
      | Ok file_full ->
        let chrono = Misc.Chrono.start() in
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
        begin match cactus_plot with
         | Error e ->
           Log.err (fun k->k "successful reply for show-gp/%S" file);
           H.Response.make_string
             (Error (500, Printf.sprintf "<failed to load cactus plot: %s>" e))
         | Ok plot ->
           Log.debug (fun k->k "encode png file of %d bytes" (String.length plot));
           Log.debug (fun k->k "successful reply for show-gp/%S" file);
           H.Response.make_string
             ~headers:H.Headers.([] |> set "content-type" "image/png")
             (Ok plot)
        end
    )

let handle_show_errors (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show-err" @/ string_urlencoded @/ return)
    (fun file _req ->
      Log.info (fun k->k "----- start show-err %s -----" file);
      let chrono = Misc.Chrono.start() in
      match Bin_utils.load_file_summary ~full:true file with
      | Error e ->
        Log.err (fun k->k "cannot find %S:\n%s" file e);
        H.Response.fail ~code:500 "could not load %S:\n%s" file e
      | Ok (_file_full, cr) ->
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
    )

(* show full table for a file *)
let handle_show_as_table (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show_table" @/ string_urlencoded @/ return)
    (fun file req ->
      let chrono = Misc.Chrono.start() in
      let params = H.Request.query req in
      let offset = try List.assoc "offset" params |> int_of_string with Not_found -> 0 in
      let page_size = 25 in
      Bin_utils.with_file_as_db file (fun _scope db ->
        let full_table =
          let link_res prover pb ~res =
            PB.link ~uri:(uri_show_single file prover pb) (PB.text res)
          in
          Test.Top_result.db_to_printbox_table ~offset ~link_pb:link_get_file
            ~page_size ~link_res db
        in
        Log.info (fun k->k "loaded table[offset=%d] in %.3fs"
                     offset (Misc.Chrono.since_last chrono));
        let h =
          let open Html in
           (* pagination buttons *)
          (* FIXME: only display next if not complete *)
           let btns = List.flatten [
              (if offset > 0 then (
                  [mk_a ~cls:["btn-info";"btn-sm"]
                     ~a:[a_href
                           (uri_show_table ~offset:(max 0 (offset-page_size)) file)]
                     [txt "prev"]]
                ) else []);
               [
                 mk_a ~cls:["btn-info";"btn-sm"]
                   ~a:[a_href
                         (uri_show_table ~offset:(offset+page_size) file)]
                   [txt "next"]
               ];
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
            [h3 [txt "full results"];
             div [pb_html full_table]];
          ]
        in
        Log.debug (fun k->k "successful reply for %S" file);
        H.Response.make_string (Ok (Html.to_string h))
        )
      |> E.catch
        ~ok:(fun r -> r)
        ~err:(fun e ->
          Log.err (fun k->k"exn: %s" e);
          H.Response.make (Error (500, "failure: " ^ e)))
    )

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

(* show list of individual results with URLs to single results for a file *)
let handle_show_detailed (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show_detailed" @/ string_urlencoded @/ return)
    (fun db_file req ->
      let params = H.Request.query req in
      let offset = try List.assoc "offset" params |> int_of_string with Not_found -> 0 in
      let filter_res = try List.assoc "res" params |> U.percent_encode with Not_found -> "" in
      let filter_prover = try List.assoc "prover" params |> U.percent_encode with Not_found -> "" in
      let filter_pb = try List.assoc "pb" params |> U.percent_encode with Not_found -> "" in
      let page_size = 25 in
      Log.debug (fun k->k "-- show detailed file=%S offset=%d pb=`%s` res=`%s` prover=`%s` --"
                     db_file offset filter_pb filter_res filter_prover);
      Bin_utils.with_file_as_db db_file
        (fun scope db ->
           let l, complete =
             Test.Detailed_res.list_keys
               ~page_size ~offset ~filter_prover ~filter_res ~filter_pb db
             |> scope.unwrap in
           Log.debug (fun k->k "got %d results, complete=%B" (List.length l) complete);
           let open Html in
           (* pagination buttons *)
           let btns = List.flatten [
               (if offset > 0 then (
                   [mk_a ~cls:["btn-info";"btn-sm"]
                      ~a:[a_href
                            (uri_show_detailed ~offset:(max 0 (offset-page_size))
                               ~filter_res ~filter_pb ~filter_prover db_file)]
                      [txt "prev"]]
                 ) else []);
               (if complete then [] else [
                   mk_a ~cls:["btn-info";"btn-sm"]
                     ~a:[a_href
                           (uri_show_detailed ~offset:(offset+page_size)
                              ~filter_res ~filter_pb ~filter_prover db_file)]
                     [txt "next"]
                 ]);
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
              h3 [txt "detailed results"];
              form ~a:[a_action (uri_show_detailed db_file);
                       a_method `Get;
                       a_class ["form-row"]]
                [
                  input ~a:[a_name "prover"; a_class ["form-control-sm"];
                            a_value filter_prover;
                            a_placeholder "prover";
                            a_input_type `Text] ();
                  input ~a:[a_name "pb"; a_class ["form-control-sm"];
                            a_value filter_pb;
                            a_placeholder "problem";
                            a_input_type `Text] ();
                  input ~a:[a_name "res"; a_class ["form-control-sm"];
                            a_value filter_res;
                            a_placeholder "result";
                            a_input_type `Text] ();
                  mk_button ~cls:["btn-info";"btn-sm";"form-control-sm"]
                    [txt "filter"];
                ];
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
              table ~a:[a_class ["framed"]] ~thead rows;
             ];
           ]
        )
      |> E.catch
        ~ok:(fun h ->
            Log.debug (fun k->k "successful reply for %S" db_file);
            H.Response.make_string (Ok (Html.to_string h)))
        ~err:(fun e ->
            Log.err (fun k->k "error in show-detailed %S:\n%s" db_file e);
            H.Response.fail ~code:500 "could not show detailed res for %S:\n%s" db_file e)
    )

(* show invidual result *)
let handle_show_single (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show_single" @/ string_urlencoded @/
             string_urlencoded @/ string_urlencoded @/ return)
    (fun db_file prover pb_file _req ->
      Log.debug (fun k->k "show single called with prover=%s, pb_file=%s" prover pb_file);
      Bin_utils.with_file_as_db db_file
        (fun scope db ->
           let r = Test.Detailed_res.get_res db prover pb_file |> scope.unwrap in
           let pb, stdout, stderr =
             Test.Detailed_res.to_printbox ~link:(fun _ -> link_get_file) r
           in
           let open Html in
           mk_page ~title:"single result" [
             mk_navigation [
               uri_show db_file, "file", false;
               uri_show_detailed db_file, "detailed", false;
               uri_show_single db_file prover pb_file, "single", true;
             ];
             dyn_status self;
             mk_a
               ~cls:["sticky-top"]
               ~a:[ a_href (Printf.sprintf "/show_detailed/%s" (U.percent_encode db_file))]
               [txt "back to detailed results"];
             h3 [txt @@ Printf.sprintf "results for %s on %s" prover pb_file];
             div [pb_html pb];
             details (summary ~a:[a_class ["alert";"alert-secondary"]] [txt "full stdout"])
               [pre [txt stdout]];
             details (summary ~a:[a_class ["alert";"alert-secondary"]] [txt "full stderr"])
               [pre [txt stderr]];
           ]
        )
      |> E.catch
        ~ok:(fun h ->
            Log.debug (fun k->k "successful reply for %S" db_file);
            H.Response.make_string (Ok (Html.to_string h)))
        ~err:(fun e ->
            Log.err (fun k->k "error in show-single %S:\n%s" db_file e);
            H.Response.fail ~code:500 "could not show single res for %S:\n%s" db_file e)
    )

(* export as CSV *)
let handle_show_csv (self:t): unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show_csv" @/ string_urlencoded @/ return)
    (fun db_file req ->
      Bin_utils.with_file_as_db db_file
        (fun _scope db ->
          let query = H.Request.query req in
          Log.debug
            (fun k->k  "query: [%s]"
                (String.concat ";"  @@
                 CCList.map (fun (x,y) -> Printf.sprintf "%S=%S" x y) query));
          let provers =
            try Some (List.assoc "provers" query |> CCString.split_on_char ',')
            with _ -> None
          in
          let csv = Test.Top_result.db_to_csv_string ?provers db in
          Log.debug (fun k->k "successful reply for /show_csv/%S" db_file);
          H.Response.make_string
            ~headers:["Content-Type", "plain/csv";
                      "Content-Disposition", "attachment; filename=\"results.csv\""]
            (Ok csv))
      |> E.catch
        ~ok:(fun h ->
            Log.debug (fun k->k "successful reply for %S" db_file);
            h)
        ~err:(fun e ->
            Log.err (fun k->k "error in show-single %S:\n%s" db_file e);
            H.Response.fail ~code:500 "could not show single res for %S:\n%s" db_file e)
    )

(* compare files *)
let handle_compare self : unit =
  let server = self.server in
  H.add_route_handler server ~meth:`POST
    H.Route.(exact "compare" @/ return)
    (fun req ->
      let body = H.Request.body req |> String.trim in
      Log.debug (fun k->k "/compare: body is %s" body);
      let body = U.parse_query body |> E.get_or_failwith in
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
        H.Response.make_string (Ok (Html.to_string h))
      ) else (
        H.Response.fail ~code:412 "precondition failed: select at least 2 files"
      )
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
    let h = html_redirect ~href:"/" @@ Format.asprintf "deleted %d files" (List.length files) in
    H.Response.make_string (Ok (Html.to_string h))
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
    (fun req ->
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
      H.Response.make_string (Ok (Html.to_string h))
    )

let handle_tasks (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "tasks" @/ return)
    (fun _r ->
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
      H.Response.make_string (Ok (Html.to_string h))
    )

let handle_run (self:t) : unit =
  H.add_route_handler self.server ~meth:`POST
    H.Route.(exact "run" @/ string_urlencoded @/ return)
    (fun name _r ->
      Log.debug (fun k->k "run task %S" name);
      let task =
        match Definitions.find_task self.defs name with
        | Ok t -> t
        | Error e -> H.Response.fail_raise ~code:404 "cannot find task %s: %s" name e
      in
      Log.debug (fun k->k "found task %s, run it" name);
      Task_queue.push self.task_q task;
      let msg =
        Format.asprintf "task queued (%d in queue)!" (Task_queue.size self.task_q)
      in
      H.Response.make_string @@ Ok (Html.to_string @@ html_redirect ~href:"/" msg)
    )

let handle_job_interrupt (self:t) : unit =
  H.add_route_handler self.server ~meth:`POST
    H.Route.(exact "interrupt" @/ return)
    (fun _r ->
      Log.debug (fun k->k "interrupt current task");
      let r =
        match Task_queue.cur_job self.task_q with
        | None -> Ok (Html.to_string @@ html_redirect ~href:"/" "no job to interrupt.")
        | Some j ->
          Task_queue.Job.interrupt j;
          Ok (Html.to_string @@ html_redirect ~href:"/" "job interrupted")
      in
      H.Response.make_string r
    )

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
    (fun _req ->
      let entries = Bin_utils.list_entries self.data_dir in
      let chrono = Misc.Chrono.start() in
      let h =
        let open Html in
        mk_page ~title:"benchpress"
          [
            h1 [txt "Benchpress"];
            dyn_status self;
            h3 [txt "configuration"];
            ul ~a:[a_class ["list-group"]] @@ List.flatten [
              [li ~a:[a_class ["list-group-item"]]
                 [mk_a ~a:[a_href "/provers/"] [txt "provers"]];
               li ~a:[a_class ["list-group-item"]]
                 [mk_a ~a:[a_href "/tasks/"] [txt "tasks"]]];
            ];
            h3 [txt "list of results"];
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
            form ~a:[a_id (uri_of_string "compare"); a_method `Post;] [
              div ~a:[a_class ["container"]] [
                mk_row ~cls:["sticky-top"; "justify-self-center"; "w-50";] @@
                List.flatten [
                  [ mk_col [ mk_button ~cls:["btn-primary";"btn-sm"]
                               ~a:[a_formaction "/compare/"] [txt "compare
                               selected"]];
                  ];
                  if self.allow_delete then [
                    mk_col [
                      mk_button ~cls:["btn-danger";"btn-sm"] ~a:[a_formaction "/delete/"]
                        [txt "delete selected"]]
                  ] else [];
                ];
                mk_ul l
              ]];
          ]
      in
      H.Response.make_string (Ok (Html.to_string h))
    )

let handle_file_summary (self:t) : unit =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "file-sum" @/ string_urlencoded @/ return)
    (fun file _req ->
      let chrono = Misc.Chrono.start() in
      match Bin_utils.mk_file_full file with
      | Error _e ->
        H.Response.fail_raise ~code:404 "file %s not found" file
      | Ok file_full ->
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
        Log.debug (fun k->k "reply to handle-file-summary %s in %.3fs"
                     file (Misc.Chrono.since_last chrono));
        H.Response.make_string (Ok (Html.to_string_elt h))
    )

let handle_task_status self =
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "tasks_status" @/ return)
    (fun _req ->
      let open Html in
      let bod =
        mk_ul @@ List.flatten [
          [mk_li [
              txt @@
              Format.asprintf "jobs in queue: %d" (Task_queue.size self.task_q)]];
          begin match Task_queue.cur_job self.task_q with
            | None -> []
            | Some j ->
              (* display current job *)
              [mk_li [
                  div ~a:[a_class ["spinner-border"; "spinner-border-sm"]] [span []];
                  pre [txt @@
                       Format.asprintf "current task: %a" Task_queue.Job.pp j];
                  form ~a:[a_id (uri_of_string "cancel");
                           a_action (uri_of_string "/interrupt/");
                           a_method `Post;]
                    [mk_button ~cls:["btn-warning"] [txt "interrupt"]]]
              ];
          end
        ]
      in
      let html = mk_page ~title:"tasks_status" [div [bod]] in
      H.Response.make_string (Ok (Html.to_string html))
    );
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "api" @/ exact "tasks_status" @/ return)
    (fun _req ->
      let j =
        Task_queue.status self.task_q
        |> Task_queue.status_to_json
      in
      H.Response.make_string ~headers:["Content-Type", "text/json"] (Ok j)
    );
  ()

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
            H.Response.fail_raise ~code:500 "cannot open file %S:\n%s" file
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
      Printf.printf "listen on http://localhost:%d/\n%!" (H.port server);
      handle_root self;
      handle_task_status self;
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

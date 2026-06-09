open Common
include Tiny_httpd_html

let b_style = link [ A.rel "stylesheet"; A.href "/css/" ]

let mk_page_ ?meta:(my_meta = []) ~title:my_title my_body =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "html.mk-page" in
  html []
    [
      head []
        [
          title [] [ txt my_title ];
          b_style;
          PB_html.style;
          link [ A.rel "icon"; A.href "/favicon.png" ];
          meta (A.charset "utf-8" :: my_meta);
          meta
            [
              A.name "viewport"; A.content "width=device-width, initial-scale=1";
            ];
          script [ A.src "/js"; "type", "module" ] [ txt "" ];
          script [ A.src "/htmx.js" ] [ txt "" ];
          script [ A.src "/echarts.js" ] [ txt "" ];
          script [ A.src "/htmx-echarts.js" ] [ txt "" ];
        ];
      body [ "hx-ext", "echarts" ] [ my_body ];
    ]

let mk_page ?meta ~title my_body =
  mk_page_ ?meta ~title @@ div [ A.class_ "container" ] my_body

let mk_page' ?meta ~title my_body =
  mk_page_ ?meta ~title @@ div' [ A.class_ "container" ] my_body

let div1 a x = div a [ x ]
let mk_a ?(cls = "btn-link") al x = a (A.class_ ("btn " ^ cls) :: al) x
let mk_row ?(cls = "") al x = div ((A.class_ @@ "row " ^ cls) :: al) x
let mk_col ?(cls = "") al x = div ((A.class_ @@ "col " ^ cls) :: al) x
let mk_li al x = li (A.class_ "list-group-item" :: al) x
let mk_ul al l = ul (A.class_ "list-group" :: al) l

let mk_button ?(cls = "") al x =
  button (A.type_ "submit" :: A.class_ ("btn " ^ cls) :: al) x

(** [hx-…] attribute *)
let a_hx key v = "hx-" ^ key, v

let pb_html pb = div [ A.class_ "table" ] [ PB_html.to_html pb ]
let to_string_elt h = to_string ~top:false h
let to_string = to_string_top

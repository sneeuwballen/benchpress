open Common
module B = PrintBox
module H = Tiny_httpd_html
module A = H.A

let style =
  let l =
    [
      "table.framed { border: 2px solid black; }";
      "table.framed th, table.framed td { border: 1px solid black; }";
      "th, td { padding: 3px; }";
    ]
  in
  H.style [] (CCList.map H.txt l)

let attrs_of_style (s : B.Style.t) : _ list * _ =
  let open B.Style in
  let { bold; bg_color; fg_color; _ } = s in
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
    (match bg_color with
    | None -> []
    | Some c -> [ "background-color", encode_color c ])
    @
    match fg_color with
    | None -> []
    | Some c -> [ "color", encode_color c ]
  in
  let a =
    match s with
    | [] -> []
    | s ->
      [
        A.style @@ String.concat ";"
        @@ CCList.map (fun (k, v) -> k ^ ": " ^ v) s;
      ]
  in
  a, bold

let rec to_html_rec (b : B.t) : H.elt =
  match B.view b with
  | B.Empty -> H.span [] []
  | B.Text { l; style } ->
    let a, bold = attrs_of_style style in
    let l = CCList.map H.txt l in
    let l =
      if bold then
        CCList.map (fun x -> H.b [] [ x ]) l
      else
        l
    in
    H.div a l
  | B.Pad (_, b) | B.Frame { sub = b; _ } -> to_html_rec b
  | B.Align { h = `Right; inner = b; v = _ } ->
    H.div [ A.class_ "align-right" ] [ to_html_rec b ]
  | B.Align { h = `Center; inner = b; v = _ } ->
    H.div [ A.class_ "center" ] [ to_html_rec b ]
  | B.Align { inner = b; _ } -> to_html_rec b
  | B.Grid (bars, a) ->
    let class_ =
      match bars with
      | `Bars -> "table-bordered framed"
      | `None -> "table-borderless"
    in
    let to_row a =
      Array.to_list a
      |> CCList.map (fun b -> H.td [ A.class_ "thead" ] [ to_html_rec b ])
      |> fun x -> H.tr [] x
    in
    let rows = Array.to_list a |> CCList.map to_row in
    H.table [ A.class_ @@ "table table-hover table-striped " ^ class_ ] rows
  | B.Tree (_, b, l) ->
    let l = Array.to_list l in
    H.div []
      [
        to_html_rec b;
        H.ul [] (CCList.map (fun x -> H.li [] [ to_html_rec x ]) l);
      ]
  | B.Link { uri; inner } ->
    H.div [] [ H.a [ A.class_ "btn-link"; A.href uri ] [ to_html_rec inner ] ]
  | _ ->
    (* catch-all to be more resilient to newer versions of printbox *)
    H.div [] [ H.pre [] [ H.txt @@ PrintBox_text.to_string b ] ]
(* remaining cases *)
[@@warning "-11"]

let to_html b =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "printbox.to_html" in
  H.div [] [ to_html_rec b ]

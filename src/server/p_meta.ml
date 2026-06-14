open Common
module H = Tiny_httpd
module U = Tiny_httpd.Util
module PB = PrintBox
module Log = (val Logs.src_log (Logs.Src.create "benchpress-serve"))

let handle_user_meta_get (self : Server_common.t) : unit =
  let open Server_common in
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "user-meta" @/ string_urlencoded @/ return)
  @@ fun file _req ->
  let@ _chrono = query_wrap (Error.wrapf "serving /user-meta/%s" file) in
  let html =
    try
      let entries =
        with_result_db_read file ~f:(fun db ->
            Test_metadata.get_all_user_meta db)
      in
      render_user_meta_html file entries
    with Error.E e | E (e, _) ->
      Html.div [ Html.A.id "user-meta" ] [ Html.txt (Error.show e) ]
  in
  H.Response.make_string ~headers:default_html_headers
    (Ok (Html.to_string_elt html))

let handle_user_meta_post (self : Server_common.t) : unit =
  let open Server_common in
  H.add_route_handler self.server ~meth:`POST
    H.Route.(exact "user-meta" @/ string_urlencoded @/ return)
  @@ fun file req ->
  let@ _chrono = query_wrap (Error.wrapf "POST /user-meta/%s" file) in
  let body = H.Request.body req |> String.trim in
  let params =
    U.parse_query body
    |> Misc.unwrap_str (fun () -> spf "parse-query failed on %s" body)
  in
  let key =
    try List.assoc "key" params
    with Not_found -> failf 400 "missing 'key' parameter"
  in
  let value =
    try List.assoc "value" params
    with Not_found -> failf 400 "missing 'value' parameter"
  in
  if String.trim key = "" then failf 400 "key must not be empty";
  with_result_db_write file ~f:(fun db ->
      Test_metadata.set_user_meta db ~key ~value);
  let entries =
    with_result_db_read file ~f:(fun db -> Test_metadata.get_all_user_meta db)
  in
  let html = render_user_meta_html file entries in
  H.Response.make_string ~headers:default_html_headers
    (Ok (Html.to_string_elt html))

let handle_user_meta_delete (self : Server_common.t) : unit =
  let open Server_common in
  H.add_route_handler self.server ~meth:`DELETE
    H.Route.(
      exact "user-meta" @/ string_urlencoded @/ string_urlencoded @/ return)
  @@ fun file key _req ->
  let@ _chrono = query_wrap (Error.wrapf "DELETE /user-meta/%s/%s" file key) in
  with_result_db_write file ~f:(fun db ->
      Test_metadata.delete_user_meta db ~key);
  let entries =
    with_result_db_read file ~f:(fun db -> Test_metadata.get_all_user_meta db)
  in
  let html = render_user_meta_html file entries in
  H.Response.make_string ~headers:default_html_headers
    (Ok (Html.to_string_elt html))

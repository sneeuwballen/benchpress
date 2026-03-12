let bearer_of_req req =
  match Tiny_httpd.Request.get_header req "authorization" with
  | None -> None
  | Some v ->
    (* "Bearer <token>" *)
    if String.length v > 7 && String.sub v 0 7 = "Bearer " then
      Some (String.sub v 7 (String.length v - 7))
    else
      None

let authenticate auth req =
  match bearer_of_req req with
  | None -> Error "missing Authorization: Bearer header"
  | Some key ->
    (match Auth.authenticate auth ~key with
     | Some user_id -> Ok user_id
     | None -> Error "invalid API key")

let require_auth auth req ~send_resp =
  match authenticate auth req with
  | Ok user_id -> Some user_id
  | Error msg ->
    let body = Printf.sprintf {|{"error":%S}|} msg in
    let resp =
      Tiny_httpd.Response.make_string
        ~code:401
        ~headers:Tiny_httpd.Headers.([] |> set "content-type" "application/json")
        (Ok body)
    in
    send_resp resp;
    None

(* Hmap key used to store the authenticated user_id on the HTTP request. *)
let user_meta_key : string Hmap.key = Hmap.Key.create ()

let middleware auth : Tiny_httpd.Middleware.t =
  fun handler req ~resp ->
    match authenticate auth req with
    | Error msg ->
      let body = Printf.sprintf {|{"msg":%S,"code":"unauthenticated"}|} msg in
      resp
        (Tiny_httpd.Response.make_string ~code:401
           ~headers:
             Tiny_httpd.Headers.([] |> set "content-type" "application/json")
           (Ok body))
    | Ok user_id ->
      Tiny_httpd.Request.add_meta req user_meta_key user_id;
      handler req ~resp

let user_of_req req = Tiny_httpd.Request.get_meta req user_meta_key

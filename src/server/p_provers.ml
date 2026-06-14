open Common
module H = Tiny_httpd
module U = Tiny_httpd.Util
module PB = PrintBox
module Log = (val Logs.src_log (Logs.Src.create "benchpress-serve"))

let handle_provers (self : Server_common.t) : unit =
  let open Server_common in
  H.add_route_handler self.server ~meth:`GET H.Route.(exact "provers" @/ return)
  @@ fun req ->
  let@ _chrono = query_wrap (Error.wrap "serving /provers/") in
  let name =
    try Some (List.assoc "name" @@ H.Request.query req) with _ -> None
  in
  let provers =
    match name with
    | Some name ->
      (match Definitions.find_prover' self.defs name with
      | p -> [ p ]
      | exception Error.E e ->
        H.Response.fail_raise ~code:404 "no such prover: %s" @@ Error.show e)
    | None -> Definitions.all_provers self.defs |> List.map With_loc.view
  in
  let h =
    let open Html in
    let mk_prover p =
      mk_li [] [ pre [] [ txt @@ Format.asprintf "@[<v>%a@]" Prover.pp p ] ]
    in
    let l = CCList.map mk_prover provers in
    mk_page ~title:"provers"
      [
        mk_navigation [ "/provers/", "provers", true ];
        h3 [] [ txt "list of provers" ];
        mk_ul [] l;
      ]
  in
  H.Response.make_string ~headers:default_html_headers (Ok (Html.to_string h))

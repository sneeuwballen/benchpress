(* run tests, or compare results *)
open Logitest
module T = Test
module E = CCResult

module H = Tiny_httpd
module U = Tiny_httpd_util

module Html = Tyxml_html

(* start from http://bettermotherfuckingwebsite.com/ and added some *)
let basic_css = {|
  body{margin:44px auto;font-family: monospace;
  max-width:1024px;font-size:18px;color:#444;padding:10px}
  h1,h2,h3{line-height:1.2} table {width: 100%;} .framed {border-width:3px; border-style: solid}
  |}

let string_of_html h = Format.asprintf "@[%a@]@." (Html.pp ()) h

(* show individual files *)
let handle_show server : unit =
  H.add_path_handler server ~meth:`GET "/show/%s%!" (fun file _req ->
      match Show.load_file file with
      | Error e ->
        H.Response.fail ~code:500 "could not load %S:\n%s" file e
      | Ok res ->
        let box = Test.Top_result.(to_printbox res) in
        let h =
          let open Html in
          html
            (head (title (txt "show")) [style [txt basic_css]])
            (body [
                a ~a:[a_href "/"] [txt "back to root"];
                h3 [txt file];
                div [PrintBox_html.to_html box];
            ])
        in
        H.Response.make_string (Ok (string_of_html h))
    )

(* compare files *)
let handle_compare server : unit =
  H.add_path_handler server ~meth:`POST "/compare" (fun req ->
      let body = H.Request.body req |> String.trim in
      Misc.Debug.debugf 4 (fun k->k "/compare: body is %s" body);
      let names =
        CCString.split_on_char '&' body
        |> CCList.filter_map
          (fun s -> match CCString.Split.left ~by:"=" (String.trim s) with
             | Some (name, "on") -> Some name
             | _ -> None)
      in
      Misc.Debug.debugf 2 (fun k->k "/compare: names is [%s]" @@ String.concat ";" names);
      if List.length names>=2 then (
        let files =
          names
          |> List.map
            (fun s -> match Show.load_file s with
               | Error e -> H.Response.fail_raise ~code:404 "invalid file %S: %s" s e
               | Ok x -> s, x)
        in
        let box_compare_l =
          let open PrintBox in
          CCList.diagonal files
          |> List.map (fun ((f1,r1),(f2,r2)) ->
              let c = Test.Top_result.compare r1 r2 in
              vlist ~bars:false [
                text f1; text f2;
                Test.Top_result.comparison_to_printbox ~short:true c])
          |> vlist
        in
        let h =
          let open Html in
          html
            (head (title (txt "compare")) [style [txt basic_css]])
            (body [
                a ~a:[a_href "/"] [txt "back to root"];
                h3 [txt "comparison"];
                div [PrintBox_html.to_html box_compare_l];
              ])
        in
        H.Response.make_string (Ok (string_of_html h))
      ) else (
        H.Response.fail ~code:412 "precondition failed: select at least 2 files"
      )
    )

(* index *)
let handle_root server data_dir : unit =
  H.add_path_handler server ~meth:`GET "/%!" (fun _req ->
      let entries = Utils.list_entries data_dir in
      let h =
        let open Html in
        html
          (head(title (txt "list")) [style [txt basic_css]])
          (body [
              h3 [txt "list of results"];
              let l = 
                List.map
                  (fun (s,size) ->
                     let s = Filename.basename s in
                     let href =
                       Printf.sprintf "/show/%s" (U.percent_encode ~skip:(fun c->c='/') s)
                     in
                     li [
                       a ~a:[a_href href] [txt s];
                       txt (Printf.sprintf "(%s)" (Misc.human_size size));
                       input ~a:[a_input_type `Checkbox; a_name s] ()
                     ])
                  entries
              in
              form ~a:[a_id (uri_of_string "compare");
                       a_action (uri_of_string "/compare/");
                       a_method `Post]
                [button ~a:[a_button_type `Submit] [txt "compare selected"]; ul l];
            ])
      in
      H.Response.make_string (Ok (string_of_html h))
    )

let main ~debug ?port () =
  try
    let server = H.create ?port () in
    if debug >0 then (
      H._enable_debug true;
      Misc.Debug.set_level debug;
    );
    Printf.printf "listen on http://localhost:%d/\n%!" (H.port server);
    let data_dir = Filename.concat (Xdg.data_dir()) "logitest" in
    handle_root server data_dir;
    handle_show server;
    handle_compare server;
    H.run server |> E.map_err Printexc.to_string
  with e ->
    E.of_exn_trace e




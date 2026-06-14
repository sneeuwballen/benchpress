open Common
module H = Tiny_httpd
module U = Tiny_httpd.Util
module PB = PrintBox
module Log = (val Logs.src_log (Logs.Src.create "benchpress-serve"))

(* ECharts JSON for cactus plot *)
let handle_show_echarts (self : Server_common.t) : unit =
  let open Server_common in
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "show-echarts" @/ string_urlencoded @/ return)
  @@ fun q_arg _req ->
  let@ _chrono = query_wrap (Error.wrapf "serving /show-echarts/%s" q_arg) in
  Log.debug (fun k -> k "----- start show-echarts %s -----" q_arg);
  let files = CCString.split_on_char ',' q_arg |> List.map String.trim in
  let files_full =
    CCList.map
      (fun file ->
        match CCString.split_on_char '/' file with
        | [ file; prover ] -> Bin_utils.mk_file_full file, Some [ prover ]
        | _ -> Bin_utils.mk_file_full file, None)
      files
  in
  let plot =
    match files_full with
    | [ (f, _provers) ] -> Cactus_plot.of_file f
    | fs ->
      fs
      |> List.mapi (fun i (file, provers) ->
             guardf 500 (Error.wrapf "building cactus plot for %s" file)
             @@ fun () ->
             let p = Cactus_plot.of_file ?provers file in
             spf "file %d (%s)" i (Filename.basename file), p)
      |> Cactus_plot.combine
  in
  let json = Cactus_plot.to_echarts_json plot in
  Log.debug (fun k -> k "successful reply for show-echarts/%S" q_arg);
  H.Response.make_string
    ~headers:H.Headers.([] |> set "content-type" "application/json")
    (Ok json)

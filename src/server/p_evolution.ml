open Common
module H = Tiny_httpd
module U = Tiny_httpd.Util
module PB = PrintBox
module Log = (val Logs.src_log (Logs.Src.create "benchpress-serve"))

(** ECharts JSON for evolution plot of similar runs *)
let handle_evolution (self : Server_common.t) : unit =
  let open Server_common in
  H.add_route_handler self.server ~meth:`GET
    H.Route.(exact "evolution" @/ string_urlencoded @/ return)
  @@ fun file _req ->
  let@ _chrono = query_wrap (Error.wrapf "serving /evolution/%s" file) in
  Log.debug (fun k -> k "----- start evolution %s -----" file);
  let file_full = Bin_utils.mk_file_full file in
  let similar = Meta_cache.find_similar_runs self.meta_cache file_full in
  Log.debug (fun k ->
      k "found %d similar runs for %s" (List.length similar) file_full);
  let data_points =
    List.filter_map
      (fun path ->
        try
          let meta = Meta_cache.find self.meta_cache path in
          let summary =
            if Bin_utils.is_sqlite_file path then
              Db.with_db ~timeout:500 ~mode:`READONLY path per_prover_summary
            else if Misc.is_zst_file path then
              Bin_utils.with_decompressed_zst path (fun tmp ->
                  Db.with_db ~timeout:500 ~mode:`READONLY tmp per_prover_summary)
            else
              failf 400 "unsupported file format for '%s'" path
          in
          Some (meta.Test_metadata.timestamp, summary)
        with _ -> None)
      similar
  in
  let data_points =
    List.sort
      (fun (t1, _) (t2, _) ->
        match t1, t2 with
        | Some t1, Some t2 -> Float.compare t1 t2
        | Some _, None -> -1
        | None, Some _ -> 1
        | None, None -> 0)
      data_points
  in
  let timestamps =
    List.map
      (fun (ts, _) ->
        match ts with
        | None -> "unknown"
        | Some f ->
          let tm = Unix.localtime f in
          Printf.sprintf "%02d/%02d %02d:%02d" tm.Unix.tm_mon tm.Unix.tm_mday
            tm.Unix.tm_hour tm.Unix.tm_min)
      data_points
  in
  let all_provers =
    List.fold_left
      (fun acc (_, summary) ->
        List.fold_left
          (fun acc s ->
            if List.mem s.prover acc then
              acc
            else
              s.prover :: acc)
          acc summary)
      [] data_points
    |> List.rev
  in
  let categories =
    [
      ("sat", fun s -> s.sat);
      ("unsat", fun s -> s.unsat);
      ("error", fun s -> s.error);
      ("bad", fun s -> s.bad);
    ]
  in
  let series =
    List.concat_map
      (fun prover ->
        List.map
          (fun (cat, pick) ->
            `Assoc
              [
                "name", `String (prover ^ " " ^ cat);
                "type", `String "line";
                ( "data",
                  `List
                    (List.map
                       (fun (_, summary) ->
                         match
                           List.find_opt
                             (fun s -> String.equal s.prover prover)
                             summary
                         with
                         | Some s -> `Int (pick s)
                         | None -> `Int 0)
                       data_points) );
              ])
          categories)
      all_provers
  in
  let legend_data =
    List.concat_map
      (fun prover ->
        List.map (fun (cat, _) -> `String (prover ^ " " ^ cat)) categories)
      all_provers
  in
  let option =
    `Assoc
      [
        ( "title",
          `Assoc
            [
              "text", `String "Evolution of similar runs";
              ( "subtext",
                `String
                  (let n = List.length data_points in
                   Printf.sprintf "%d run%s with matching file sets" n
                     (if n = 1 then
                        ""
                      else
                        "s")) );
            ] );
        "tooltip", `Assoc [ "trigger", `String "axis" ];
        "legend", `Assoc [ "data", `List legend_data ];
        ( "xAxis",
          `Assoc
            [
              "type", `String "category";
              "data", `List (List.map (fun s -> `String s) timestamps);
              "name", `String "run date";
            ] );
        "yAxis", `Assoc [ "type", `String "value"; "name", `String "count" ];
        "series", `List series;
      ]
  in
  let json = Yojson.Basic.to_string option in
  Log.debug (fun k -> k "successful reply for evolution/%S" file);
  H.Response.make_string
    ~headers:H.Headers.([] |> set "content-type" "application/json")
    (Ok json)

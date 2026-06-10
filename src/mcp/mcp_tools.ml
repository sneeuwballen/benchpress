(** Benchpress MCP tool implementations. Uses proto-generated types for all
    response serialization. *)

module T = Benchpress_mcp_types
module J = Json_helpers

let resolve_file ~data_dir file = Filename.concat data_dir file

(** -- list_jobs -- *)

let tool_list_jobs ~data_dir : Mcp.tool_def =
  let input_schema =
    `Assoc
      [ "type", `String "object";
        "properties",
        `Assoc
          [ "limit",
            `Assoc [ "type", `String "integer";
                     "description", `String "Maximum jobs (default 10)" ];
            "offset",
            `Assoc [ "type", `String "integer";
                     "description", `String "Offset for pagination (default 0)" ];
          ] ]
  in
  let handler (args : Yojson.Basic.t) : Yojson.Basic.t =
    let limit = J.assoc_int_or "limit" ~default:10 args in
    let offset = J.assoc_int_or "offset" ~default:0 args in
    let entries, _more = Bin_utils.list_entries data_dir ~off:offset ~limit in
    let jobs =
      List.map
        (fun (file_path, size) ->
          let basename = Filename.basename file_path in
          let meta =
            try
              let _file, cr = Bin_utils.load_file_summary ~full:false basename in
              Some cr.cr_meta
            with _ -> None
          in
          let uuid_str = match meta with Some m -> Uuidm.to_string m.uuid | None -> "" in
          let ts = match meta with Some m -> CCOpt.get_or ~default:0. m.timestamp | None -> 0. in
          let n_res = match meta with Some m -> Int32.of_int m.n_results | None -> 0l in
          let n_b = match meta with Some m -> Int32.of_int m.n_bad | None -> 0l in
          let provers = match meta with Some m -> m.provers | None -> [] in
          T.make_job_summary ~file:basename ~uuid:uuid_str ~timestamp:ts
            ~n_results:n_res ~n_bad:n_b ~provers ~size_bytes:(Int32.of_int size) ())
        entries
    in
    T.encode_json_list_jobs_result
      (T.make_list_jobs_result ~jobs ~offset:(Int32.of_int offset)
         ~limit:(Int32.of_int limit) ())
  in
  { name = "list_jobs";
    description = "List recently run benchmarks (jobs) from benchpress.";
    input_schema; handler; }

(** -- get_job_status -- *)

let tool_get_job_status ~data_dir : Mcp.tool_def =
  let input_schema =
    `Assoc
      [ "type", `String "object";
        "properties",
        `Assoc
          [ "file",
            `Assoc [ "type", `String "string";
                     "description", `String "The benchmark file name" ] ] ;
        "required", `List [ `String "file" ] ]
  in
  let handler (args : Yojson.Basic.t) : Yojson.Basic.t =
    let file = J.assoc_string_or "file" ~default:"" args in
    if file = "" then
      `Assoc [ "error", `String "missing 'file' parameter" ]
    else
      let _file_full, cr =
        Bin_utils.load_file_summary ~full:false (resolve_file ~data_dir file)
      in
      let meta = cr.cr_meta in
      let provers =
        List.map
          (fun (p_name, stat) ->
            let open Test_stat in
            T.make_prover_stat ~prover:p_name
              ~sat:(Int32.of_int stat.sat.n)
              ~unsat:(Int32.of_int stat.unsat.n)
              ~errors:(Int32.of_int stat.errors)
              ~timeout:(Int32.of_int stat.timeout)
              ~unknown:(Int32.of_int stat.unknown.n)
              ~memory:(Int32.of_int stat.memory)
              ~total:(Int32.of_int stat.total)
              ~total_time:stat.total_time
              ~total_time_solved:stat.total_time_solved ())
          cr.cr_stat
      in
      T.encode_json_job_status_result
        (T.make_job_status_result ~file
           ~uuid:(Uuidm.to_string meta.uuid)
           ~timestamp:(CCOpt.get_or ~default:0. meta.timestamp)
           ~total_wall_time:(CCOpt.get_or ~default:0. meta.total_wall_time)
           ~n_results:(Int32.of_int meta.n_results)
           ~n_bad:(Int32.of_int meta.n_bad)
           ~is_complete:(CCOpt.is_some meta.total_wall_time)
           ~provers ())
  in
  { name = "get_job_status";
    description = "Get the status and per-prover breakdown of a benchpress job.";
    input_schema; handler; }

(** -- get_job_results -- *)

let tool_get_job_results ~data_dir : Mcp.tool_def =
  let input_schema =
    `Assoc
      [ "type", `String "object";
        "properties",
        `Assoc
          [ "file",
            `Assoc [ "type", `String "string";
                     "description", `String "The benchmark file name" ] ] ;
        "required", `List [ `String "file" ] ]
  in
  let handler (args : Yojson.Basic.t) : Yojson.Basic.t =
    let file = J.assoc_string_or "file" ~default:"" args in
    if file = "" then
      `Assoc [ "error", `String "missing 'file' parameter" ]
    else
      Bin_utils.with_file_as_db
        ~map_err:(Error.wrapf "using DB '%s'" file)
        (resolve_file ~data_dir file)
        (fun db ->
          let table = Test_top_result.db_to_table db in
          let rows =
            List.map
              (fun (row : Test_top_result.table_row) ->
                let results =
                  List.map
                    (fun (prover, res, rtime) ->
                      T.make_result_entry ~prover ~result:(Res.to_string res)
                        ~time_s:rtime ())
                    row.tr_res
                in
                T.make_result_row ~problem:row.tr_problem ~results ())
              table.t_rows
          in
          T.encode_json_job_results_result
            (T.make_job_results_result ~file ~provers:table.t_provers ~rows
               ~n_rows:(Int32.of_int (List.length table.t_rows)) ()))
  in
  { name = "get_job_results";
    description = "Get the full results table for a benchmark job.";
    input_schema; handler; }

(** -- query_job_results -- *)

let tool_query_job_results ~data_dir : Mcp.tool_def =
  let input_schema =
    `Assoc
      [ "type", `String "object";
        "properties",
        `Assoc
          [ "file",
            `Assoc [ "type", `String "string";
                     "description", `String "The benchmark file name" ];
            "prover",
            `Assoc [ "type", `String "string";
                     "description", `String "Filter by prover name (partial match)" ];
            "problem",
            `Assoc [ "type", `String "string";
                     "description", `String "Filter by problem file (partial match)" ];
            "result",
            `Assoc [ "type", `String "string";
                     "description", `String "Filter: sat, unsat, error, timeout, unknown" ];
            "page",
            `Assoc [ "type", `String "integer";
                     "description", `String "Page number (0-based, default 0)" ];
            "per_page",
            `Assoc [ "type", `String "integer";
                     "description", `String "Results per page (default 25)" ] ] ;
        "required", `List [ `String "file" ] ]
  in
  let handler (args : Yojson.Basic.t) : Yojson.Basic.t =
    let file = J.assoc_string_or "file" ~default:"" args in
    if file = "" then
      `Assoc [ "error", `String "missing 'file' parameter" ]
    else
      Bin_utils.with_file_as_db
        ~map_err:(Error.wrapf "using DB '%s'" file)
        (resolve_file ~data_dir file)
        (fun db ->
          let open Test_detailed_res in
          let filter_prover = J.assoc_string_or "prover" ~default:"" args in
          let filter_pb = J.assoc_string_or "problem" ~default:"" args in
          let filter_res = J.assoc_string_or "result" ~default:"" args in
          let page = J.assoc_int_or "page" ~default:0 args in
          let per_page = J.assoc_int_or "per_page" ~default:25 args in
          let keys, total, is_complete =
            list_keys db ~offset:(page * per_page) ~page_size:per_page
              ~filter_prover ~filter_pb ~filter_res
          in
          let items =
            List.map
              (fun (k : key) ->
                T.make_query_item ~prover:k.prover ~problem:k.file
                  ~result:(Res.to_string k.res)
                  ~expected:(Res.to_string k.file_expect) ~time_s:k.rtime ())
              keys
          in
          T.encode_json_query_job_results_result
            (T.make_query_job_results_result ~file ~total:(Int32.of_int total)
               ~page:(Int32.of_int page) ~per_page:(Int32.of_int per_page)
               ~is_complete ~items ()))
  in
  { name = "query_job_results";
    description = "Paginated, filterable query over individual benchmark results.";
    input_schema; handler; }

(** Register all MCP tools into the given registry. *)
let register_all ~(reg : Mcp.t) ~(data_dir : string) : unit =
  Mcp.register reg ~tool:(tool_list_jobs ~data_dir);
  Mcp.register reg ~tool:(tool_get_job_status ~data_dir);
  Mcp.register reg ~tool:(tool_get_job_results ~data_dir);
  Mcp.register reg ~tool:(tool_query_job_results ~data_dir)

(* run tests, or compare results *)

let run _defs files =
  let res_l = CCList.map Bin_utils.load_file_full files in
  List.iter
    (fun (file, res) ->
      Logs.app (fun k ->
          k "convert to sql file %S (uuid %a)" file Uuidm.pp
            res.Test_top_result.meta.uuid);
      let file = Filename.chop_suffix file ".json.gz" ^ ".sqlite" in
      Logs.app (fun k -> k "sql file is %S" file);
      Exec_action.dump_results_sqlite res)
    res_l

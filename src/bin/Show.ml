(* run tests, or compare results *)

let main ?(check = true) ?(bad = true) ~details ~csv ?csv_file ~jsonl ?jsonl_file
    ?summary (file : string) : unit =
  Logs.debug (fun k -> k "loading file %S..." file);

  (* Only show default output if not exporting to a specific format *)
  let is_exporting = csv || jsonl || CCOption.is_some csv_file || CCOption.is_some jsonl_file in

  Bin_utils.with_loaded_file
    ~map_err:(Error.wrapf "loading file %S" file)
    file
    (fun res ->
      Logs.debug (fun k -> k "loaded file %S" file);
      Bin_utils.dump_csv_stdout ~csv res;
      Bin_utils.dump_csv_file ~csv_file res;
      Bin_utils.dump_jsonl ~jsonl res;
      Bin_utils.dump_jsonl_file ~jsonl_file res;
      Bin_utils.dump_summary ~summary res;

      if not is_exporting then (
        Bin_utils.printbox_results ~details res;
        if bad && not (Test_top_result.is_ok res) then
          Format.printf "@[<2>bad: %a@]@." Test_top_result.pp_bad res
      );

      if check then Bin_utils.check_res Notify.nil res;
      ())

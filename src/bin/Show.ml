
(* run tests, or compare results *)
module T = Test
module E = CCResult

let main ?(check=true) ?(bad=true) ?csv ?summary files =
  let open E.Infix in
  E.map_l Utils.load_file files >>= fun res ->
  let results = T.Top_result.merge_l res in
  Utils.dump_csv ~csv results;
  Utils.dump_summary ~summary results;
  Utils.printbox_results results;
  if bad then (
    Format.printf "@[<2>bad: %a@]@." T.Top_result.pp_bad results;
  );
  if check then Utils.check_res Notify.nil results else E.return ()



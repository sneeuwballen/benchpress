
(* run tests, or compare results *)
module T = Test
module E = CCResult

let main ?(check=true) ?(bad=true) ?csv ?summary (file:string) =
  let open E.Infix in
  Utils.load_file file >>= fun res ->
  Utils.dump_csv ~csv res;
  Utils.dump_summary ~summary res;
  Utils.printbox_results res;
  if bad then (
    Format.printf "@[<2>bad: %a@]@." T.Top_result.pp_bad res;
  );
  if check then Utils.check_res Notify.nil res else E.return ()



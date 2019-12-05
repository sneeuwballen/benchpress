
(* run tests, or compare results *)
open Logitest
module T = Test
module E = CCResult

let load_file (f:string) : (T.Top_result.t, _) E.t =
  try
    let dir = Filename.concat (Xdg.data_dir()) "logitest" in
    let file = Filename.concat dir f in
    if not @@ Sys.file_exists file then (
      Error ("cannot find file " ^ f)
    ) else (
      if Filename.check_suffix f ".gz" then (
        (* use [zcat] to decompress *)
        let v =
          CCUnix.with_process_in (Printf.sprintf "zcat '%s'" file)
            ~f:Misc.Json.J.from_channel in
        (* Format.printf "%a@." (Misc.Json.J.pretty_print ?std:None) v; *)
        Misc.Json.Decode.decode_value T.Top_result.decode v
        |> E.map_err Misc.Json.Decode.string_of_error
      ) else (
        Misc.Json.Decode.decode_file T.Top_result.decode file
        |> E.map_err Misc.Json.Decode.string_of_error
      )
    )
  with e ->
    E.of_exn_trace e

let main ?(check=true) ?(bad=true) ?csv ?summary files =
  let open E.Infix in
  E.map_l load_file files >>= fun res ->
  let results = T.Top_result.merge_l res in
  Utils.dump_csv ~csv results;
  Utils.dump_summary ~summary results;
  Utils.printbox_results results;
  if bad then (
    Format.printf "@[<2>bad: %a@]@." T.Top_result.pp_bad results;
  );
  if check then Utils.check_res Notify.nil results else E.return ()




(* run tests, or compare results *)
open Logitest
module T = Test
module E = CCResult
module Db = Sqlite3_utils

let run _defs files =
  Misc.err_with (fun scope ->
      let res_l = List.map (fun x -> scope.unwrap @@ Utils.load_file_full x) files in
      List.iter
        (fun (file, res) ->
           Misc.Debug.debugf 1 (fun k->k "convert to sql file %S" file);
           let file = (Filename.chop_suffix file ".json.gz") ^ ".sqlite" in
           Misc.Debug.debugf 2 (fun k->k "sql file is %S" file);
           Db.with_db file
             (fun db -> T.Top_result.to_db db res |> scope.unwrap)
        ) res_l)
  |> E.catch
    ~ok:(fun () -> Ok ())
    ~err:(fun e -> Error (Format.asprintf "error: %s@." e))


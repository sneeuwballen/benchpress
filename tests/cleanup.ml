let () =
  let file = Sys.argv.(1) in
  let lines =
    CCIO.with_in file @@ fun ic ->
    CCIO.read_lines_l ic
    |> CCList.filter (fun s ->
           (not (CCString.mem ~sub:"reading" s))
           && not (CCString.mem ~sub:"in file" s))
  in
  CCIO.with_out file (fun oc -> CCIO.write_lines_l oc lines)

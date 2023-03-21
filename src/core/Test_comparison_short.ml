open Common
open Test
module PB = PrintBox

type single = { better: int; worse: int; same: int }

type t = {
  provers: Prover.name list;
  tbl: (Prover.name * Prover.name * single) list;
}

let of_db db : t =
  (* get a single integer *)
  let db_get db s x1 x2 =
    let l =
      Db.exec db s x1 x2
        ~ty:Db.Ty.(p2 text text, p1 int, id)
        ~f:Db.Cursor.to_list_rev
      |> Misc.unwrap_db (fun () -> "extract comparison-short from DB")
    in
    match l with
    | [ x ] -> x
    | _ -> Error.failf "expected a single integer, got %a" Fmt.(Dump.list int) l
  in
  Error.guard (Error.wrap "comparison-short.of_db") @@ fun () ->
  let provers = list_provers db in
  (* TODO: make a single query and group-by? *)
  let tbl =
    CCList.diagonal provers
    |> List.rev_map (fun (p1, p2) ->
           assert (p1 <> p2);
           let better =
             db_get db
               {|select count(r1.file) from prover_res r1, prover_res r2
                  where r1.prover=? and r2.prover=? and r1.file=r2.file
                  and r1.res in ('sat','unsat') and not (r2.res in ('sat','unsat')); |}
               p1 p2
           and worse =
             db_get db
               {|select count(r1.file) from prover_res r1, prover_res r2
                  where r1.prover=? and r2.prover=? and r1.file=r2.file
                  and not (r1.res in ('sat','unsat')) and (r2.res in ('sat','unsat')); |}
               p1 p2
           and same =
             db_get db
               {|select count(r1.file) from prover_res r1, prover_res r2
                  where r1.prover=? and r2.prover=? and r1.file=r2.file
                  and r1.res in ('sat','unsat') and (r2.res in ('sat','unsat')); |}
               p1 p2
           in
           p1, p2, { better; worse; same })
  in
  { provers; tbl }

exception Find_int of (int * int * int)

let to_printbox_l_ (self : t) =
  let get_pair p1 p2 =
    try
      List.iter
        (fun (p1', p2', r) ->
          if p1 = p1' && p2 = p2' then
            raise (Find_int (r.better, r.worse, r.same))
          else if p1 = p2' && p2 = p1' then
            raise (Find_int (r.worse, r.better, r.same)))
        self.tbl;
      assert false
    with Find_int x -> x
  in
  let headers =
    PB.text "better:"
    :: List.map (PB.text_with_style PB.Style.bold) self.provers
  in
  let tbl =
    List.map
      (fun p2 ->
        PB.text p2
        :: List.map
             (fun p1 ->
               if p1 = p2 then
                 PB.center_hv @@ PB.text "×"
               else (
                 let bet, worse, same = get_pair p1 p2 in
                 (* TODO: URL for detailed comparison p1 vs p2 *)
                 let tsf s x = PB.text @@ Printf.sprintf s x in
                 PB.center_hv
                 @@ PB.vlist ~bars:false
                      [
                        tsf "same: %d" same;
                        tsf "better: %d" bet;
                        tsf "worse: %d" worse;
                      ]
               ))
             self.provers)
      self.provers
  in
  PB.grid_l ~bars:true (headers :: tbl)
(* TODO: grid display (-> to array, then by index + reverse when i<j?)
   let to_printbox_grid l : PB.t =
*)

let to_printbox_l self =
  if List.length self.provers >= 2 then
    to_printbox_l_ self
  else
    PB.empty
(* nothing interesting *)

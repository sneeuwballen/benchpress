
(** Lightweight Results *)

open Common

(** A kind of lightweight result *)
type t = {
  cr_meta: Test_metadata.t;
  cr_stat: (Prover.name * Test_stat.t) list;
  cr_analyze: (Prover.name * Test_analyze.t) list;
  cr_comparison: Test_comparison_short.t;
}

let of_db ?full db : t =
  Profile.with_ "compact-res.of-db" @@ fun () ->
  Error.guard (Error.wrap "reading compact results") @@ fun () ->
  Db.transact db (fun _ ->
    let cr_meta = Test_metadata.of_db db in
    let cr_stat = Test_stat.of_db db in
    let cr_analyze = Test_analyze.of_db ?full db in
    let cr_comparison = Test_comparison_short.of_db db in
    {cr_stat; cr_analyze; cr_comparison; cr_meta; })

let pp out _self = Fmt.fprintf out "<compact result>"

(** Lightweight Results *)

open Common

type t = {
  cr_meta: Test_metadata.t;
  cr_stat: (Prover.name * Test_stat.t) list;
  cr_analyze: (Prover.name * Test_analyze.t) list;
  cr_comparison: Test_comparison_short.t;
}
(** A kind of lightweight result *)

let of_events ?(full = false) ~meta (events : Run_event.t list) : t =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "compact-res.of-events" in
  let cr_stat = Test_stat.of_events events in
  let cr_analyze = Test_analyze.of_events ~full events in
  let cr_comparison = Test_comparison_short.of_events events in
  { cr_stat; cr_analyze; cr_comparison; cr_meta = meta }

let pp out (_self : _ lazy_t * t) = Fmt.fprintf out "<compact result>"

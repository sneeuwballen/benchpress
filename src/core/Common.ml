module Fmt = CCFormat
module Db = Sqlite3_utils
module CCOpt = Containers.Option
module Trace = Trace_core
module OT = Opentelemetry_trace

let spf = Printf.sprintf
let ( let@ ) = ( @@ )

let with_span ~__FILE__ ~__LINE__ name f =
  Trace.with_span ~__FILE__ ~__LINE__ name @@ fun span ->
  Trace_wrap_.wrap_span_ span @@ fun () -> f span

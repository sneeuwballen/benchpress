module Fmt = CCFormat
module J = Misc.Json

type +'a t = {
  program : 'a;
  problem : Problem.t;
  res : Res.t;
  timeout : int;
  raw : Proc_run_result.t;
}

let program e = e.program
let problem e = e.problem
let raw e = e.raw

let map ~f e = { e with program = f e.program }

let analyze_self_ (self:Prover.t t) =
  let res =
    match Prover.analyze_p_opt self.program self.raw with
    | Some x -> x
    | None ->
      if self.raw.errcode = 0 then Res.Unknown
      else if self.raw.rtime > float self.timeout then Res.Timeout
      else Res.Error
  in
  { self with res }

let analyze_self self =
  self |> analyze_self_ |> map ~f:Prover.name

let make_from_prover (p:Prover.t) ~timeout problem (raw:Proc_run_result.t) : Prover.name t =
  { program=p; problem; res=Res.Unknown; timeout; raw }
  |> analyze_self

let make (p:Prover.name) ~timeout ~res problem (raw:Proc_run_result.t) : _ t =
  { program=p; problem; res; timeout; raw }

let pp pp_prog out (self:_ t): unit =
  Format.fprintf out "(@[<hv2>:program %a@ :problem %a@ :raw %a@ :res %s@])"
    pp_prog (program self) Problem.pp (problem self) Proc_run_result.pp (raw self)
    (Res.to_string self.res)

let decode f =
  let open J.Decode in
  field "problem" Problem.decode >>= fun problem ->
  field "timeout" int >>= fun timeout ->
  field "program" f >>= fun program ->
  field_opt "res" string >>= fun res ->
  let res = CCOpt.map_or ~default:Res.Unknown Res.of_string res in
  field "raw" Proc_run_result.decode >>= fun raw ->
  succeed {problem;timeout;program;raw;res}

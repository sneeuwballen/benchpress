module Fmt = CCFormat

type (+'a, 'res) t = {
  program : 'a;
  problem : Problem.t;
  res : 'res;
  timeout : Limit.Time.t;
  raw : Run_proc_result.t;
}

let program e = e.program
let problem e = e.problem
let raw e = e.raw

let map ~f e = { e with program = f e.program }

let float_timeout t = Limit.Time.as_float Seconds t

let analyze_self_ (self:(Prover.t, Res.t) t) =
  let res =
    match Prover.analyze_p_opt self.program self.raw with
    | Some x -> x
    | None ->
      if self.raw.errcode = 0 then Res.Unknown
      else if self.raw.rtime > float_timeout self.timeout then Res.Timeout
      else Res.Error
  in
  { self with res }

let analyze_self self =
  self |> analyze_self_ |> map ~f:Prover.name

let make_from_prover (p:Prover.t) ~timeout problem
    (raw:Run_proc_result.t) : (Prover.name, Res.t) t =
  { program=p; problem; res=Res.Unknown; timeout; raw }
  |> analyze_self

let make_from_checker
    (p:Prover.t) (checker:Proof_checker.t) ~timeout problem raw : _ t =
  let module Res = Proof_check_res in
  let res = match Proof_checker.analyze_res checker raw with
    | Some r -> r
    | None ->
      if raw.errcode = 0 then Res.Unknown "errcode <> 0"
      else if raw.rtime > float_timeout timeout then Res.Unknown "timeout"
      else Res.Unknown "?"
  in
  { program=(p.name,checker.name); problem; res; timeout; raw; }

let make (p:_) ~timeout ~res problem (raw:Run_proc_result.t) : _ t =
  { program=p; problem; res; timeout; raw }

let pp pp_prog pp_res out (self:_ t): unit =
  Format.fprintf out "(@[<hv2>:program %a@ :problem %a@ :raw %a@ :res %a@])"
    pp_prog (program self) Problem.pp (problem self) Run_proc_result.pp (raw self)
    pp_res self.res

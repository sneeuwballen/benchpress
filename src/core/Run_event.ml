(* This file is free software. See file "license" for more details. *)

(** {1 Event Stored on Disk or Transmitted on Network} *)

open Common
module Log = (val Logs.src_log (Logs.Src.create "benchpress.run-event"))

type prover = Prover.t
type checker = Proof_checker.t

type t =
  | Prover_run of (Prover.name, Res.t) Run_result.t
  | Checker_run of
      (Prover.name * Proof_checker.name, Proof_check_res.t) Run_result.t

type event = t

let pp out = function
  | Prover_run r -> Run_result.pp Fmt.string Res.pp out r
  | Checker_run r ->
    let pptuple out (p, c) = Fmt.fprintf out "prover:%s, checker: %s" p c in
    Run_result.pp pptuple Proof_check_res.pp out r

let mk_prover r = Prover_run r
let mk_checker r = Checker_run r

(* main schema for results! *)
(* --- Protobuf / JSON encoding --- *)

let pb_of_problem (pb : Problem.t) : Benchpress_core.problem =
  Benchpress_core.make_problem ~name:pb.Problem.name
    ~expected:(Res.to_string pb.Problem.expected)
    ()

let problem_of_pb (p : Benchpress_core.problem) : Problem.t =
  {
    Problem.name = p.Benchpress_core.name;
    expected = Res.of_string ~tags:[] p.Benchpress_core.expected;
  }

let pb_of_raw ~no_data (raw : Run_proc_result.t) :
    Benchpress_core.run_proc_result =
  let blob s =
    if no_data then
      Benchpress_core.Sha256ref (Blob_ref.sha256_hex (Bytes.of_string s))
    else
      Blob_ref.of_string s
  in
  Benchpress_core.make_run_proc_result ~errcode:(Int32.of_int raw.errcode)
    ~stdout:(blob raw.stdout) ~stderr:(blob raw.stderr) ~rtime:raw.rtime
    ~utime:raw.utime ~stime:raw.stime ()

let raw_of_pb ~read_zip_entry (r : Benchpress_core.run_proc_result) :
    Run_proc_result.t =
  let get_blob = function
    | None -> ""
    | Some dr -> Blob_ref.to_string ~read_zip_entry dr
  in
  {
    errcode = Int32.to_int r.Benchpress_core.errcode;
    stdout = get_blob r.Benchpress_core.stdout;
    stderr = get_blob r.Benchpress_core.stderr;
    rtime = r.Benchpress_core.rtime;
    utime = r.Benchpress_core.utime;
    stime = r.Benchpress_core.stime;
  }

let to_event_pb ?(no_data = false) (self : t) : Benchpress_core.event =
  match self with
  | Prover_run r ->
    let res = r.Run_result.res in
    let labels =
      match res with
      | Res.Tag s -> [ s ]
      | _ -> []
    in
    Benchpress_core.Prover_done
      (Benchpress_core.make_prover_done ~prover:r.Run_result.program
         ~problem:(pb_of_problem r.Run_result.problem)
         ~res:(Res.to_string res) ~labels
         ~timeout_s:
           (Int64.of_int (Limit.Time.as_int Seconds r.Run_result.timeout))
         ~raw:(pb_of_raw ~no_data r.Run_result.raw)
         ())
  | Checker_run r ->
    let p, c = r.Run_result.program in
    Benchpress_core.Checker_done
      (Benchpress_core.make_checker_done ~prover:p ~checker:c
         ~problem:(pb_of_problem r.Run_result.problem)
         ~proof_check_res:(Proof_check_res.to_string r.Run_result.res)
         ~raw:(pb_of_raw ~no_data r.Run_result.raw)
         ())

let of_event_pb ~read_zip_entry (ev : Benchpress_core.event) : t option =
  match ev with
  | Benchpress_core.Prover_done pd ->
    let pb = Option.map problem_of_pb pd.Benchpress_core.problem in
    Option.map
      (fun problem ->
        Prover_run
          (Run_result.make pd.Benchpress_core.prover
             ~timeout:
               (Limit.Time.mk ~s:(Int64.to_int pd.Benchpress_core.timeout_s) ())
             ~res:(Res.of_string ~tags:[] pd.Benchpress_core.res)
             problem
             (Option.fold
                ~none:
                  Run_proc_result.
                    {
                      errcode = 0;
                      stdout = "";
                      stderr = "";
                      rtime = 0.;
                      utime = 0.;
                      stime = 0.;
                    }
                ~some:(raw_of_pb ~read_zip_entry)
                pd.Benchpress_core.raw)))
      pb
  | Benchpress_core.Checker_done cd ->
    let pb = Option.map problem_of_pb cd.Benchpress_core.problem in
    Option.map
      (fun problem ->
        Checker_run
          (Run_result.make
             (cd.Benchpress_core.prover, cd.Benchpress_core.checker)
             ~timeout:(Limit.Time.mk ~s:0 ())
             ~res:(Proof_check_res.of_string cd.Benchpress_core.proof_check_res)
             problem
             (Option.fold
                ~none:
                  Run_proc_result.
                    {
                      errcode = 0;
                      stdout = "";
                      stderr = "";
                      rtime = 0.;
                      utime = 0.;
                      stime = 0.;
                    }
                ~some:(raw_of_pb ~read_zip_entry)
                cd.Benchpress_core.raw)))
      pb
  | _ -> None

let to_json_line ?(no_data = false) (self : t) : string =
  let ev = to_event_pb ~no_data self in
  Yojson.Basic.to_string (Benchpress_core.encode_json_event ev)

let of_json_line ~read_zip_entry (line : string) : t option =
  let json = Yojson.Basic.from_string line in
  let ev = Benchpress_core.decode_json_event json in
  of_event_pb ~read_zip_entry ev

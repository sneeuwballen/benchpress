[@@@ocaml.warning "-23-27-30-39-44"]

type problem = {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 2 fields *)
  mutable name: string;
  mutable expected: string;
}

type data_ref = Inline of bytes | Sha256ref of string

type run_proc_result = {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 4 fields *)
  mutable errcode: int32;
  mutable stdout: data_ref option;
  mutable stderr: data_ref option;
  mutable rtime: float;
  mutable utime: float;
  mutable stime: float;
}

type run_start = {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 2 fields *)
  mutable uuid: string;
  mutable timestamp: float;
  mutable dirs: string list;
  mutable provers: string list;
}

type run_done = {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 3 fields *)
  mutable total_wall_time: float;
  mutable n_results: int32;
  mutable n_bad: int32;
}

type prover_start = {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 2 fields *)
  mutable prover: string;
  mutable problem: problem option;
  mutable timeout_s: int64;
}

type prover_done = {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 3 fields *)
  mutable prover: string;
  mutable problem: problem option;
  mutable res: string;
  mutable labels: string list;
  mutable timeout_s: int64;
  mutable raw: run_proc_result option;
}

type checker_start = {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 2 fields *)
  mutable prover: string;
  mutable checker: string;
  mutable problem: problem option;
}

type checker_done = {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 3 fields *)
  mutable prover: string;
  mutable checker: string;
  mutable problem: problem option;
  mutable proof_check_res: string;
  mutable raw: run_proc_result option;
}

type manifest = {
  mutable _presence: Pbrt.Bitfield.t;  (** presence for 5 fields *)
  mutable uuid: string;
  mutable timestamp: string;
  mutable provers: string list;
  mutable total_wall_time: float;
  mutable n_results: int32;
  mutable n_bad: int32;
  mutable dirs: string list;
}

type event =
  | Run_start of run_start
  | Run_done of run_done
  | Prover_start of prover_start
  | Prover_done of prover_done
  | Checker_start of checker_start
  | Checker_done of checker_done

let default_problem () : problem =
  { _presence = Pbrt.Bitfield.empty; name = ""; expected = "" }

let default_data_ref () : data_ref = Inline (Bytes.create 0)

let default_run_proc_result () : run_proc_result =
  {
    _presence = Pbrt.Bitfield.empty;
    errcode = 0l;
    stdout = None;
    stderr = None;
    rtime = 0.;
    utime = 0.;
    stime = 0.;
  }

let default_run_start () : run_start =
  {
    _presence = Pbrt.Bitfield.empty;
    uuid = "";
    timestamp = 0.;
    dirs = [];
    provers = [];
  }

let default_run_done () : run_done =
  {
    _presence = Pbrt.Bitfield.empty;
    total_wall_time = 0.;
    n_results = 0l;
    n_bad = 0l;
  }

let default_prover_start () : prover_start =
  {
    _presence = Pbrt.Bitfield.empty;
    prover = "";
    problem = None;
    timeout_s = 0L;
  }

let default_prover_done () : prover_done =
  {
    _presence = Pbrt.Bitfield.empty;
    prover = "";
    problem = None;
    res = "";
    labels = [];
    timeout_s = 0L;
    raw = None;
  }

let default_checker_start () : checker_start =
  { _presence = Pbrt.Bitfield.empty; prover = ""; checker = ""; problem = None }

let default_checker_done () : checker_done =
  {
    _presence = Pbrt.Bitfield.empty;
    prover = "";
    checker = "";
    problem = None;
    proof_check_res = "";
    raw = None;
  }

let default_manifest () : manifest =
  {
    _presence = Pbrt.Bitfield.empty;
    uuid = "";
    timestamp = "";
    provers = [];
    total_wall_time = 0.;
    n_results = 0l;
    n_bad = 0l;
    dirs = [];
  }

let default_event () : event = Run_start (default_run_start ())

(** {2 Make functions} *)

let[@inline] problem_has_name (self : problem) : bool =
  Pbrt.Bitfield.get self._presence 0

let[@inline] problem_has_expected (self : problem) : bool =
  Pbrt.Bitfield.get self._presence 1

let[@inline] problem_set_name (self : problem) (x : string) : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 0;
  self.name <- x

let[@inline] problem_set_expected (self : problem) (x : string) : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 1;
  self.expected <- x

let copy_problem (self : problem) : problem = { self with name = self.name }

let make_problem ?(name : string option) ?(expected : string option) () :
    problem =
  let _res = default_problem () in
  (match name with
  | None -> ()
  | Some v -> problem_set_name _res v);
  (match expected with
  | None -> ()
  | Some v -> problem_set_expected _res v);
  _res

let[@inline] run_proc_result_has_errcode (self : run_proc_result) : bool =
  Pbrt.Bitfield.get self._presence 0

let[@inline] run_proc_result_has_stdout (self : run_proc_result) : bool =
  self.stdout != None

let[@inline] run_proc_result_has_stderr (self : run_proc_result) : bool =
  self.stderr != None

let[@inline] run_proc_result_has_rtime (self : run_proc_result) : bool =
  Pbrt.Bitfield.get self._presence 1

let[@inline] run_proc_result_has_utime (self : run_proc_result) : bool =
  Pbrt.Bitfield.get self._presence 2

let[@inline] run_proc_result_has_stime (self : run_proc_result) : bool =
  Pbrt.Bitfield.get self._presence 3

let[@inline] run_proc_result_set_errcode (self : run_proc_result) (x : int32) :
    unit =
  self._presence <- Pbrt.Bitfield.set self._presence 0;
  self.errcode <- x

let[@inline] run_proc_result_set_stdout (self : run_proc_result) (x : data_ref)
    : unit =
  self.stdout <- Some x

let[@inline] run_proc_result_set_stderr (self : run_proc_result) (x : data_ref)
    : unit =
  self.stderr <- Some x

let[@inline] run_proc_result_set_rtime (self : run_proc_result) (x : float) :
    unit =
  self._presence <- Pbrt.Bitfield.set self._presence 1;
  self.rtime <- x

let[@inline] run_proc_result_set_utime (self : run_proc_result) (x : float) :
    unit =
  self._presence <- Pbrt.Bitfield.set self._presence 2;
  self.utime <- x

let[@inline] run_proc_result_set_stime (self : run_proc_result) (x : float) :
    unit =
  self._presence <- Pbrt.Bitfield.set self._presence 3;
  self.stime <- x

let copy_run_proc_result (self : run_proc_result) : run_proc_result =
  { self with errcode = self.errcode }

let make_run_proc_result ?(errcode : int32 option) ?(stdout : data_ref option)
    ?(stderr : data_ref option) ?(rtime : float option) ?(utime : float option)
    ?(stime : float option) () : run_proc_result =
  let _res = default_run_proc_result () in
  (match errcode with
  | None -> ()
  | Some v -> run_proc_result_set_errcode _res v);
  (match stdout with
  | None -> ()
  | Some v -> run_proc_result_set_stdout _res v);
  (match stderr with
  | None -> ()
  | Some v -> run_proc_result_set_stderr _res v);
  (match rtime with
  | None -> ()
  | Some v -> run_proc_result_set_rtime _res v);
  (match utime with
  | None -> ()
  | Some v -> run_proc_result_set_utime _res v);
  (match stime with
  | None -> ()
  | Some v -> run_proc_result_set_stime _res v);
  _res

let[@inline] run_start_has_uuid (self : run_start) : bool =
  Pbrt.Bitfield.get self._presence 0

let[@inline] run_start_has_timestamp (self : run_start) : bool =
  Pbrt.Bitfield.get self._presence 1

let[@inline] run_start_set_uuid (self : run_start) (x : string) : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 0;
  self.uuid <- x

let[@inline] run_start_set_timestamp (self : run_start) (x : float) : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 1;
  self.timestamp <- x

let[@inline] run_start_set_dirs (self : run_start) (x : string list) : unit =
  self.dirs <- x

let[@inline] run_start_set_provers (self : run_start) (x : string list) : unit =
  self.provers <- x

let copy_run_start (self : run_start) : run_start =
  { self with uuid = self.uuid }

let make_run_start ?(uuid : string option) ?(timestamp : float option)
    ?(dirs = []) ?(provers = []) () : run_start =
  let _res = default_run_start () in
  (match uuid with
  | None -> ()
  | Some v -> run_start_set_uuid _res v);
  (match timestamp with
  | None -> ()
  | Some v -> run_start_set_timestamp _res v);
  run_start_set_dirs _res dirs;
  run_start_set_provers _res provers;
  _res

let[@inline] run_done_has_total_wall_time (self : run_done) : bool =
  Pbrt.Bitfield.get self._presence 0

let[@inline] run_done_has_n_results (self : run_done) : bool =
  Pbrt.Bitfield.get self._presence 1

let[@inline] run_done_has_n_bad (self : run_done) : bool =
  Pbrt.Bitfield.get self._presence 2

let[@inline] run_done_set_total_wall_time (self : run_done) (x : float) : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 0;
  self.total_wall_time <- x

let[@inline] run_done_set_n_results (self : run_done) (x : int32) : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 1;
  self.n_results <- x

let[@inline] run_done_set_n_bad (self : run_done) (x : int32) : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 2;
  self.n_bad <- x

let copy_run_done (self : run_done) : run_done =
  { self with total_wall_time = self.total_wall_time }

let make_run_done ?(total_wall_time : float option) ?(n_results : int32 option)
    ?(n_bad : int32 option) () : run_done =
  let _res = default_run_done () in
  (match total_wall_time with
  | None -> ()
  | Some v -> run_done_set_total_wall_time _res v);
  (match n_results with
  | None -> ()
  | Some v -> run_done_set_n_results _res v);
  (match n_bad with
  | None -> ()
  | Some v -> run_done_set_n_bad _res v);
  _res

let[@inline] prover_start_has_prover (self : prover_start) : bool =
  Pbrt.Bitfield.get self._presence 0

let[@inline] prover_start_has_problem (self : prover_start) : bool =
  self.problem != None

let[@inline] prover_start_has_timeout_s (self : prover_start) : bool =
  Pbrt.Bitfield.get self._presence 1

let[@inline] prover_start_set_prover (self : prover_start) (x : string) : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 0;
  self.prover <- x

let[@inline] prover_start_set_problem (self : prover_start) (x : problem) : unit
    =
  self.problem <- Some x

let[@inline] prover_start_set_timeout_s (self : prover_start) (x : int64) : unit
    =
  self._presence <- Pbrt.Bitfield.set self._presence 1;
  self.timeout_s <- x

let copy_prover_start (self : prover_start) : prover_start =
  { self with prover = self.prover }

let make_prover_start ?(prover : string option) ?(problem : problem option)
    ?(timeout_s : int64 option) () : prover_start =
  let _res = default_prover_start () in
  (match prover with
  | None -> ()
  | Some v -> prover_start_set_prover _res v);
  (match problem with
  | None -> ()
  | Some v -> prover_start_set_problem _res v);
  (match timeout_s with
  | None -> ()
  | Some v -> prover_start_set_timeout_s _res v);
  _res

let[@inline] prover_done_has_prover (self : prover_done) : bool =
  Pbrt.Bitfield.get self._presence 0

let[@inline] prover_done_has_problem (self : prover_done) : bool =
  self.problem != None

let[@inline] prover_done_has_res (self : prover_done) : bool =
  Pbrt.Bitfield.get self._presence 1

let[@inline] prover_done_has_timeout_s (self : prover_done) : bool =
  Pbrt.Bitfield.get self._presence 2

let[@inline] prover_done_has_raw (self : prover_done) : bool = self.raw != None

let[@inline] prover_done_set_prover (self : prover_done) (x : string) : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 0;
  self.prover <- x

let[@inline] prover_done_set_problem (self : prover_done) (x : problem) : unit =
  self.problem <- Some x

let[@inline] prover_done_set_res (self : prover_done) (x : string) : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 1;
  self.res <- x

let[@inline] prover_done_set_labels (self : prover_done) (x : string list) :
    unit =
  self.labels <- x

let[@inline] prover_done_set_timeout_s (self : prover_done) (x : int64) : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 2;
  self.timeout_s <- x

let[@inline] prover_done_set_raw (self : prover_done) (x : run_proc_result) :
    unit =
  self.raw <- Some x

let copy_prover_done (self : prover_done) : prover_done =
  { self with prover = self.prover }

let make_prover_done ?(prover : string option) ?(problem : problem option)
    ?(res : string option) ?(labels = []) ?(timeout_s : int64 option)
    ?(raw : run_proc_result option) () : prover_done =
  let _res = default_prover_done () in
  (match prover with
  | None -> ()
  | Some v -> prover_done_set_prover _res v);
  (match problem with
  | None -> ()
  | Some v -> prover_done_set_problem _res v);
  (match res with
  | None -> ()
  | Some v -> prover_done_set_res _res v);
  prover_done_set_labels _res labels;
  (match timeout_s with
  | None -> ()
  | Some v -> prover_done_set_timeout_s _res v);
  (match raw with
  | None -> ()
  | Some v -> prover_done_set_raw _res v);
  _res

let[@inline] checker_start_has_prover (self : checker_start) : bool =
  Pbrt.Bitfield.get self._presence 0

let[@inline] checker_start_has_checker (self : checker_start) : bool =
  Pbrt.Bitfield.get self._presence 1

let[@inline] checker_start_has_problem (self : checker_start) : bool =
  self.problem != None

let[@inline] checker_start_set_prover (self : checker_start) (x : string) : unit
    =
  self._presence <- Pbrt.Bitfield.set self._presence 0;
  self.prover <- x

let[@inline] checker_start_set_checker (self : checker_start) (x : string) :
    unit =
  self._presence <- Pbrt.Bitfield.set self._presence 1;
  self.checker <- x

let[@inline] checker_start_set_problem (self : checker_start) (x : problem) :
    unit =
  self.problem <- Some x

let copy_checker_start (self : checker_start) : checker_start =
  { self with prover = self.prover }

let make_checker_start ?(prover : string option) ?(checker : string option)
    ?(problem : problem option) () : checker_start =
  let _res = default_checker_start () in
  (match prover with
  | None -> ()
  | Some v -> checker_start_set_prover _res v);
  (match checker with
  | None -> ()
  | Some v -> checker_start_set_checker _res v);
  (match problem with
  | None -> ()
  | Some v -> checker_start_set_problem _res v);
  _res

let[@inline] checker_done_has_prover (self : checker_done) : bool =
  Pbrt.Bitfield.get self._presence 0

let[@inline] checker_done_has_checker (self : checker_done) : bool =
  Pbrt.Bitfield.get self._presence 1

let[@inline] checker_done_has_problem (self : checker_done) : bool =
  self.problem != None

let[@inline] checker_done_has_proof_check_res (self : checker_done) : bool =
  Pbrt.Bitfield.get self._presence 2

let[@inline] checker_done_has_raw (self : checker_done) : bool =
  self.raw != None

let[@inline] checker_done_set_prover (self : checker_done) (x : string) : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 0;
  self.prover <- x

let[@inline] checker_done_set_checker (self : checker_done) (x : string) : unit
    =
  self._presence <- Pbrt.Bitfield.set self._presence 1;
  self.checker <- x

let[@inline] checker_done_set_problem (self : checker_done) (x : problem) : unit
    =
  self.problem <- Some x

let[@inline] checker_done_set_proof_check_res (self : checker_done) (x : string)
    : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 2;
  self.proof_check_res <- x

let[@inline] checker_done_set_raw (self : checker_done) (x : run_proc_result) :
    unit =
  self.raw <- Some x

let copy_checker_done (self : checker_done) : checker_done =
  { self with prover = self.prover }

let make_checker_done ?(prover : string option) ?(checker : string option)
    ?(problem : problem option) ?(proof_check_res : string option)
    ?(raw : run_proc_result option) () : checker_done =
  let _res = default_checker_done () in
  (match prover with
  | None -> ()
  | Some v -> checker_done_set_prover _res v);
  (match checker with
  | None -> ()
  | Some v -> checker_done_set_checker _res v);
  (match problem with
  | None -> ()
  | Some v -> checker_done_set_problem _res v);
  (match proof_check_res with
  | None -> ()
  | Some v -> checker_done_set_proof_check_res _res v);
  (match raw with
  | None -> ()
  | Some v -> checker_done_set_raw _res v);
  _res

let[@inline] manifest_has_uuid (self : manifest) : bool =
  Pbrt.Bitfield.get self._presence 0

let[@inline] manifest_has_timestamp (self : manifest) : bool =
  Pbrt.Bitfield.get self._presence 1

let[@inline] manifest_has_total_wall_time (self : manifest) : bool =
  Pbrt.Bitfield.get self._presence 2

let[@inline] manifest_has_n_results (self : manifest) : bool =
  Pbrt.Bitfield.get self._presence 3

let[@inline] manifest_has_n_bad (self : manifest) : bool =
  Pbrt.Bitfield.get self._presence 4

let[@inline] manifest_set_uuid (self : manifest) (x : string) : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 0;
  self.uuid <- x

let[@inline] manifest_set_timestamp (self : manifest) (x : string) : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 1;
  self.timestamp <- x

let[@inline] manifest_set_provers (self : manifest) (x : string list) : unit =
  self.provers <- x

let[@inline] manifest_set_total_wall_time (self : manifest) (x : float) : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 2;
  self.total_wall_time <- x

let[@inline] manifest_set_n_results (self : manifest) (x : int32) : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 3;
  self.n_results <- x

let[@inline] manifest_set_n_bad (self : manifest) (x : int32) : unit =
  self._presence <- Pbrt.Bitfield.set self._presence 4;
  self.n_bad <- x

let[@inline] manifest_set_dirs (self : manifest) (x : string list) : unit =
  self.dirs <- x

let copy_manifest (self : manifest) : manifest = { self with uuid = self.uuid }

let make_manifest ?(uuid : string option) ?(timestamp : string option)
    ?(provers = []) ?(total_wall_time : float option)
    ?(n_results : int32 option) ?(n_bad : int32 option) ?(dirs = []) () :
    manifest =
  let _res = default_manifest () in
  (match uuid with
  | None -> ()
  | Some v -> manifest_set_uuid _res v);
  (match timestamp with
  | None -> ()
  | Some v -> manifest_set_timestamp _res v);
  manifest_set_provers _res provers;
  (match total_wall_time with
  | None -> ()
  | Some v -> manifest_set_total_wall_time _res v);
  (match n_results with
  | None -> ()
  | Some v -> manifest_set_n_results _res v);
  (match n_bad with
  | None -> ()
  | Some v -> manifest_set_n_bad _res v);
  manifest_set_dirs _res dirs;
  _res

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Formatters} *)

let rec pp_problem fmt (v : problem) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~absent:(not (problem_has_name v))
      ~first:true "name" Pbrt.Pp.pp_string fmt v.name;
    Pbrt.Pp.pp_record_field
      ~absent:(not (problem_has_expected v))
      ~first:false "expected" Pbrt.Pp.pp_string fmt v.expected
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_data_ref fmt (v : data_ref) =
  match v with
  | Inline x -> Format.fprintf fmt "@[<hv2>Inline(@,%a)@]" Pbrt.Pp.pp_bytes x
  | Sha256ref x ->
    Format.fprintf fmt "@[<hv2>Sha256ref(@,%a)@]" Pbrt.Pp.pp_string x

let rec pp_run_proc_result fmt (v : run_proc_result) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~absent:(not (run_proc_result_has_errcode v))
      ~first:true "errcode" Pbrt.Pp.pp_int32 fmt v.errcode;
    Pbrt.Pp.pp_record_field ~first:false "stdout"
      (Pbrt.Pp.pp_option pp_data_ref)
      fmt v.stdout;
    Pbrt.Pp.pp_record_field ~first:false "stderr"
      (Pbrt.Pp.pp_option pp_data_ref)
      fmt v.stderr;
    Pbrt.Pp.pp_record_field
      ~absent:(not (run_proc_result_has_rtime v))
      ~first:false "rtime" Pbrt.Pp.pp_float fmt v.rtime;
    Pbrt.Pp.pp_record_field
      ~absent:(not (run_proc_result_has_utime v))
      ~first:false "utime" Pbrt.Pp.pp_float fmt v.utime;
    Pbrt.Pp.pp_record_field
      ~absent:(not (run_proc_result_has_stime v))
      ~first:false "stime" Pbrt.Pp.pp_float fmt v.stime
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_run_start fmt (v : run_start) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~absent:(not (run_start_has_uuid v))
      ~first:true "uuid" Pbrt.Pp.pp_string fmt v.uuid;
    Pbrt.Pp.pp_record_field
      ~absent:(not (run_start_has_timestamp v))
      ~first:false "timestamp" Pbrt.Pp.pp_float fmt v.timestamp;
    Pbrt.Pp.pp_record_field ~first:false "dirs"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_string)
      fmt v.dirs;
    Pbrt.Pp.pp_record_field ~first:false "provers"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_string)
      fmt v.provers
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_run_done fmt (v : run_done) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~absent:(not (run_done_has_total_wall_time v))
      ~first:true "total_wall_time" Pbrt.Pp.pp_float fmt v.total_wall_time;
    Pbrt.Pp.pp_record_field
      ~absent:(not (run_done_has_n_results v))
      ~first:false "n_results" Pbrt.Pp.pp_int32 fmt v.n_results;
    Pbrt.Pp.pp_record_field
      ~absent:(not (run_done_has_n_bad v))
      ~first:false "n_bad" Pbrt.Pp.pp_int32 fmt v.n_bad
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_prover_start fmt (v : prover_start) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~absent:(not (prover_start_has_prover v))
      ~first:true "prover" Pbrt.Pp.pp_string fmt v.prover;
    Pbrt.Pp.pp_record_field ~first:false "problem"
      (Pbrt.Pp.pp_option pp_problem)
      fmt v.problem;
    Pbrt.Pp.pp_record_field
      ~absent:(not (prover_start_has_timeout_s v))
      ~first:false "timeout_s" Pbrt.Pp.pp_int64 fmt v.timeout_s
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_prover_done fmt (v : prover_done) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~absent:(not (prover_done_has_prover v))
      ~first:true "prover" Pbrt.Pp.pp_string fmt v.prover;
    Pbrt.Pp.pp_record_field ~first:false "problem"
      (Pbrt.Pp.pp_option pp_problem)
      fmt v.problem;
    Pbrt.Pp.pp_record_field
      ~absent:(not (prover_done_has_res v))
      ~first:false "res" Pbrt.Pp.pp_string fmt v.res;
    Pbrt.Pp.pp_record_field ~first:false "labels"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_string)
      fmt v.labels;
    Pbrt.Pp.pp_record_field
      ~absent:(not (prover_done_has_timeout_s v))
      ~first:false "timeout_s" Pbrt.Pp.pp_int64 fmt v.timeout_s;
    Pbrt.Pp.pp_record_field ~first:false "raw"
      (Pbrt.Pp.pp_option pp_run_proc_result)
      fmt v.raw
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_checker_start fmt (v : checker_start) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~absent:(not (checker_start_has_prover v))
      ~first:true "prover" Pbrt.Pp.pp_string fmt v.prover;
    Pbrt.Pp.pp_record_field
      ~absent:(not (checker_start_has_checker v))
      ~first:false "checker" Pbrt.Pp.pp_string fmt v.checker;
    Pbrt.Pp.pp_record_field ~first:false "problem"
      (Pbrt.Pp.pp_option pp_problem)
      fmt v.problem
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_checker_done fmt (v : checker_done) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~absent:(not (checker_done_has_prover v))
      ~first:true "prover" Pbrt.Pp.pp_string fmt v.prover;
    Pbrt.Pp.pp_record_field
      ~absent:(not (checker_done_has_checker v))
      ~first:false "checker" Pbrt.Pp.pp_string fmt v.checker;
    Pbrt.Pp.pp_record_field ~first:false "problem"
      (Pbrt.Pp.pp_option pp_problem)
      fmt v.problem;
    Pbrt.Pp.pp_record_field
      ~absent:(not (checker_done_has_proof_check_res v))
      ~first:false "proof_check_res" Pbrt.Pp.pp_string fmt v.proof_check_res;
    Pbrt.Pp.pp_record_field ~first:false "raw"
      (Pbrt.Pp.pp_option pp_run_proc_result)
      fmt v.raw
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_manifest fmt (v : manifest) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~absent:(not (manifest_has_uuid v))
      ~first:true "uuid" Pbrt.Pp.pp_string fmt v.uuid;
    Pbrt.Pp.pp_record_field
      ~absent:(not (manifest_has_timestamp v))
      ~first:false "timestamp" Pbrt.Pp.pp_string fmt v.timestamp;
    Pbrt.Pp.pp_record_field ~first:false "provers"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_string)
      fmt v.provers;
    Pbrt.Pp.pp_record_field
      ~absent:(not (manifest_has_total_wall_time v))
      ~first:false "total_wall_time" Pbrt.Pp.pp_float fmt v.total_wall_time;
    Pbrt.Pp.pp_record_field
      ~absent:(not (manifest_has_n_results v))
      ~first:false "n_results" Pbrt.Pp.pp_int32 fmt v.n_results;
    Pbrt.Pp.pp_record_field
      ~absent:(not (manifest_has_n_bad v))
      ~first:false "n_bad" Pbrt.Pp.pp_int32 fmt v.n_bad;
    Pbrt.Pp.pp_record_field ~first:false "dirs"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_string)
      fmt v.dirs
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_event fmt (v : event) =
  match v with
  | Run_start x -> Format.fprintf fmt "@[<hv2>Run_start(@,%a)@]" pp_run_start x
  | Run_done x -> Format.fprintf fmt "@[<hv2>Run_done(@,%a)@]" pp_run_done x
  | Prover_start x ->
    Format.fprintf fmt "@[<hv2>Prover_start(@,%a)@]" pp_prover_start x
  | Prover_done x ->
    Format.fprintf fmt "@[<hv2>Prover_done(@,%a)@]" pp_prover_done x
  | Checker_start x ->
    Format.fprintf fmt "@[<hv2>Checker_start(@,%a)@]" pp_checker_start x
  | Checker_done x ->
    Format.fprintf fmt "@[<hv2>Checker_done(@,%a)@]" pp_checker_done x

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_problem (v : problem) encoder =
  if problem_has_name v then (
    Pbrt.Encoder.string v.name encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder
  );
  if problem_has_expected v then (
    Pbrt.Encoder.string v.expected encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder
  );
  ()

let rec encode_pb_data_ref (v : data_ref) encoder =
  match v with
  | Inline x ->
    Pbrt.Encoder.bytes x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder
  | Sha256ref x ->
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder

let rec encode_pb_run_proc_result (v : run_proc_result) encoder =
  if run_proc_result_has_errcode v then (
    Pbrt.Encoder.int32_as_varint v.errcode encoder;
    Pbrt.Encoder.key 1 Pbrt.Varint encoder
  );
  (match v.stdout with
  | Some x ->
    Pbrt.Encoder.nested encode_pb_data_ref x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder
  | None -> ());
  (match v.stderr with
  | Some x ->
    Pbrt.Encoder.nested encode_pb_data_ref x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder
  | None -> ());
  if run_proc_result_has_rtime v then (
    Pbrt.Encoder.float_as_bits64 v.rtime encoder;
    Pbrt.Encoder.key 4 Pbrt.Bits64 encoder
  );
  if run_proc_result_has_utime v then (
    Pbrt.Encoder.float_as_bits64 v.utime encoder;
    Pbrt.Encoder.key 5 Pbrt.Bits64 encoder
  );
  if run_proc_result_has_stime v then (
    Pbrt.Encoder.float_as_bits64 v.stime encoder;
    Pbrt.Encoder.key 6 Pbrt.Bits64 encoder
  );
  ()

let rec encode_pb_run_start (v : run_start) encoder =
  if run_start_has_uuid v then (
    Pbrt.Encoder.string v.uuid encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder
  );
  if run_start_has_timestamp v then (
    Pbrt.Encoder.float_as_bits64 v.timestamp encoder;
    Pbrt.Encoder.key 2 Pbrt.Bits64 encoder
  );
  Pbrt.List_util.rev_iter_with
    (fun x encoder ->
      Pbrt.Encoder.string x encoder;
      Pbrt.Encoder.key 3 Pbrt.Bytes encoder)
    v.dirs encoder;
  Pbrt.List_util.rev_iter_with
    (fun x encoder ->
      Pbrt.Encoder.string x encoder;
      Pbrt.Encoder.key 4 Pbrt.Bytes encoder)
    v.provers encoder;
  ()

let rec encode_pb_run_done (v : run_done) encoder =
  if run_done_has_total_wall_time v then (
    Pbrt.Encoder.float_as_bits64 v.total_wall_time encoder;
    Pbrt.Encoder.key 1 Pbrt.Bits64 encoder
  );
  if run_done_has_n_results v then (
    Pbrt.Encoder.int32_as_varint v.n_results encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder
  );
  if run_done_has_n_bad v then (
    Pbrt.Encoder.int32_as_varint v.n_bad encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder
  );
  ()

let rec encode_pb_prover_start (v : prover_start) encoder =
  if prover_start_has_prover v then (
    Pbrt.Encoder.string v.prover encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder
  );
  (match v.problem with
  | Some x ->
    Pbrt.Encoder.nested encode_pb_problem x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder
  | None -> ());
  if prover_start_has_timeout_s v then (
    Pbrt.Encoder.int64_as_varint v.timeout_s encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder
  );
  ()

let rec encode_pb_prover_done (v : prover_done) encoder =
  if prover_done_has_prover v then (
    Pbrt.Encoder.string v.prover encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder
  );
  (match v.problem with
  | Some x ->
    Pbrt.Encoder.nested encode_pb_problem x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder
  | None -> ());
  if prover_done_has_res v then (
    Pbrt.Encoder.string v.res encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder
  );
  Pbrt.List_util.rev_iter_with
    (fun x encoder ->
      Pbrt.Encoder.string x encoder;
      Pbrt.Encoder.key 4 Pbrt.Bytes encoder)
    v.labels encoder;
  if prover_done_has_timeout_s v then (
    Pbrt.Encoder.int64_as_varint v.timeout_s encoder;
    Pbrt.Encoder.key 5 Pbrt.Varint encoder
  );
  (match v.raw with
  | Some x ->
    Pbrt.Encoder.nested encode_pb_run_proc_result x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bytes encoder
  | None -> ());
  ()

let rec encode_pb_checker_start (v : checker_start) encoder =
  if checker_start_has_prover v then (
    Pbrt.Encoder.string v.prover encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder
  );
  if checker_start_has_checker v then (
    Pbrt.Encoder.string v.checker encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder
  );
  (match v.problem with
  | Some x ->
    Pbrt.Encoder.nested encode_pb_problem x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder
  | None -> ());
  ()

let rec encode_pb_checker_done (v : checker_done) encoder =
  if checker_done_has_prover v then (
    Pbrt.Encoder.string v.prover encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder
  );
  if checker_done_has_checker v then (
    Pbrt.Encoder.string v.checker encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder
  );
  (match v.problem with
  | Some x ->
    Pbrt.Encoder.nested encode_pb_problem x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder
  | None -> ());
  if checker_done_has_proof_check_res v then (
    Pbrt.Encoder.string v.proof_check_res encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder
  );
  (match v.raw with
  | Some x ->
    Pbrt.Encoder.nested encode_pb_run_proc_result x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder
  | None -> ());
  ()

let rec encode_pb_manifest (v : manifest) encoder =
  if manifest_has_uuid v then (
    Pbrt.Encoder.string v.uuid encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder
  );
  if manifest_has_timestamp v then (
    Pbrt.Encoder.string v.timestamp encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder
  );
  Pbrt.List_util.rev_iter_with
    (fun x encoder ->
      Pbrt.Encoder.string x encoder;
      Pbrt.Encoder.key 3 Pbrt.Bytes encoder)
    v.provers encoder;
  if manifest_has_total_wall_time v then (
    Pbrt.Encoder.float_as_bits64 v.total_wall_time encoder;
    Pbrt.Encoder.key 4 Pbrt.Bits64 encoder
  );
  if manifest_has_n_results v then (
    Pbrt.Encoder.int32_as_varint v.n_results encoder;
    Pbrt.Encoder.key 5 Pbrt.Varint encoder
  );
  if manifest_has_n_bad v then (
    Pbrt.Encoder.int32_as_varint v.n_bad encoder;
    Pbrt.Encoder.key 6 Pbrt.Varint encoder
  );
  Pbrt.List_util.rev_iter_with
    (fun x encoder ->
      Pbrt.Encoder.string x encoder;
      Pbrt.Encoder.key 7 Pbrt.Bytes encoder)
    v.dirs encoder;
  ()

let rec encode_pb_event (v : event) encoder =
  match v with
  | Run_start x ->
    Pbrt.Encoder.nested encode_pb_run_start x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder
  | Run_done x ->
    Pbrt.Encoder.nested encode_pb_run_done x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder
  | Prover_start x ->
    Pbrt.Encoder.nested encode_pb_prover_start x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder
  | Prover_done x ->
    Pbrt.Encoder.nested encode_pb_prover_done x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder
  | Checker_start x ->
    Pbrt.Encoder.nested encode_pb_checker_start x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder
  | Checker_done x ->
    Pbrt.Encoder.nested encode_pb_checker_done x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bytes encoder

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_problem d =
  let v = default_problem () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      ();
      continue__ := false
    | Some (1, Pbrt.Bytes) -> problem_set_name v (Pbrt.Decoder.string d)
    | Some (1, pk) -> Pbrt.Decoder.unexpected_payload_message "problem" 1 pk
    | Some (2, Pbrt.Bytes) -> problem_set_expected v (Pbrt.Decoder.string d)
    | Some (2, pk) -> Pbrt.Decoder.unexpected_payload_message "problem" 2 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : problem)

let rec decode_pb_data_ref d =
  let rec loop () =
    let ret : data_ref =
      match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "data_ref"
      | Some (1, _) -> (Inline (Pbrt.Decoder.bytes d) : data_ref)
      | Some (2, _) -> (Sha256ref (Pbrt.Decoder.string d) : data_ref)
      | Some (n, payload_kind) ->
        Pbrt.Decoder.skip d payload_kind;
        loop ()
    in
    ret
  in
  loop ()

let rec decode_pb_run_proc_result d =
  let v = default_run_proc_result () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      ();
      continue__ := false
    | Some (1, Pbrt.Varint) ->
      run_proc_result_set_errcode v (Pbrt.Decoder.int32_as_varint d)
    | Some (1, pk) ->
      Pbrt.Decoder.unexpected_payload_message "run_proc_result" 1 pk
    | Some (2, Pbrt.Bytes) ->
      run_proc_result_set_stdout v (decode_pb_data_ref (Pbrt.Decoder.nested d))
    | Some (2, pk) ->
      Pbrt.Decoder.unexpected_payload_message "run_proc_result" 2 pk
    | Some (3, Pbrt.Bytes) ->
      run_proc_result_set_stderr v (decode_pb_data_ref (Pbrt.Decoder.nested d))
    | Some (3, pk) ->
      Pbrt.Decoder.unexpected_payload_message "run_proc_result" 3 pk
    | Some (4, Pbrt.Bits64) ->
      run_proc_result_set_rtime v (Pbrt.Decoder.float_as_bits64 d)
    | Some (4, pk) ->
      Pbrt.Decoder.unexpected_payload_message "run_proc_result" 4 pk
    | Some (5, Pbrt.Bits64) ->
      run_proc_result_set_utime v (Pbrt.Decoder.float_as_bits64 d)
    | Some (5, pk) ->
      Pbrt.Decoder.unexpected_payload_message "run_proc_result" 5 pk
    | Some (6, Pbrt.Bits64) ->
      run_proc_result_set_stime v (Pbrt.Decoder.float_as_bits64 d)
    | Some (6, pk) ->
      Pbrt.Decoder.unexpected_payload_message "run_proc_result" 6 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : run_proc_result)

let rec decode_pb_run_start d =
  let v = default_run_start () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      (* put lists in the correct order *)
      run_start_set_provers v (List.rev v.provers);
      run_start_set_dirs v (List.rev v.dirs);
      continue__ := false
    | Some (1, Pbrt.Bytes) -> run_start_set_uuid v (Pbrt.Decoder.string d)
    | Some (1, pk) -> Pbrt.Decoder.unexpected_payload_message "run_start" 1 pk
    | Some (2, Pbrt.Bits64) ->
      run_start_set_timestamp v (Pbrt.Decoder.float_as_bits64 d)
    | Some (2, pk) -> Pbrt.Decoder.unexpected_payload_message "run_start" 2 pk
    | Some (3, Pbrt.Bytes) ->
      run_start_set_dirs v (Pbrt.Decoder.string d :: v.dirs)
    | Some (3, pk) -> Pbrt.Decoder.unexpected_payload_message "run_start" 3 pk
    | Some (4, Pbrt.Bytes) ->
      run_start_set_provers v (Pbrt.Decoder.string d :: v.provers)
    | Some (4, pk) -> Pbrt.Decoder.unexpected_payload_message "run_start" 4 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : run_start)

let rec decode_pb_run_done d =
  let v = default_run_done () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      ();
      continue__ := false
    | Some (1, Pbrt.Bits64) ->
      run_done_set_total_wall_time v (Pbrt.Decoder.float_as_bits64 d)
    | Some (1, pk) -> Pbrt.Decoder.unexpected_payload_message "run_done" 1 pk
    | Some (2, Pbrt.Varint) ->
      run_done_set_n_results v (Pbrt.Decoder.int32_as_varint d)
    | Some (2, pk) -> Pbrt.Decoder.unexpected_payload_message "run_done" 2 pk
    | Some (3, Pbrt.Varint) ->
      run_done_set_n_bad v (Pbrt.Decoder.int32_as_varint d)
    | Some (3, pk) -> Pbrt.Decoder.unexpected_payload_message "run_done" 3 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : run_done)

let rec decode_pb_prover_start d =
  let v = default_prover_start () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      ();
      continue__ := false
    | Some (1, Pbrt.Bytes) -> prover_start_set_prover v (Pbrt.Decoder.string d)
    | Some (1, pk) ->
      Pbrt.Decoder.unexpected_payload_message "prover_start" 1 pk
    | Some (2, Pbrt.Bytes) ->
      prover_start_set_problem v (decode_pb_problem (Pbrt.Decoder.nested d))
    | Some (2, pk) ->
      Pbrt.Decoder.unexpected_payload_message "prover_start" 2 pk
    | Some (3, Pbrt.Varint) ->
      prover_start_set_timeout_s v (Pbrt.Decoder.int64_as_varint d)
    | Some (3, pk) ->
      Pbrt.Decoder.unexpected_payload_message "prover_start" 3 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : prover_start)

let rec decode_pb_prover_done d =
  let v = default_prover_done () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      (* put lists in the correct order *)
      prover_done_set_labels v (List.rev v.labels);
      continue__ := false
    | Some (1, Pbrt.Bytes) -> prover_done_set_prover v (Pbrt.Decoder.string d)
    | Some (1, pk) -> Pbrt.Decoder.unexpected_payload_message "prover_done" 1 pk
    | Some (2, Pbrt.Bytes) ->
      prover_done_set_problem v (decode_pb_problem (Pbrt.Decoder.nested d))
    | Some (2, pk) -> Pbrt.Decoder.unexpected_payload_message "prover_done" 2 pk
    | Some (3, Pbrt.Bytes) -> prover_done_set_res v (Pbrt.Decoder.string d)
    | Some (3, pk) -> Pbrt.Decoder.unexpected_payload_message "prover_done" 3 pk
    | Some (4, Pbrt.Bytes) ->
      prover_done_set_labels v (Pbrt.Decoder.string d :: v.labels)
    | Some (4, pk) -> Pbrt.Decoder.unexpected_payload_message "prover_done" 4 pk
    | Some (5, Pbrt.Varint) ->
      prover_done_set_timeout_s v (Pbrt.Decoder.int64_as_varint d)
    | Some (5, pk) -> Pbrt.Decoder.unexpected_payload_message "prover_done" 5 pk
    | Some (6, Pbrt.Bytes) ->
      prover_done_set_raw v (decode_pb_run_proc_result (Pbrt.Decoder.nested d))
    | Some (6, pk) -> Pbrt.Decoder.unexpected_payload_message "prover_done" 6 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : prover_done)

let rec decode_pb_checker_start d =
  let v = default_checker_start () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      ();
      continue__ := false
    | Some (1, Pbrt.Bytes) -> checker_start_set_prover v (Pbrt.Decoder.string d)
    | Some (1, pk) ->
      Pbrt.Decoder.unexpected_payload_message "checker_start" 1 pk
    | Some (2, Pbrt.Bytes) ->
      checker_start_set_checker v (Pbrt.Decoder.string d)
    | Some (2, pk) ->
      Pbrt.Decoder.unexpected_payload_message "checker_start" 2 pk
    | Some (3, Pbrt.Bytes) ->
      checker_start_set_problem v (decode_pb_problem (Pbrt.Decoder.nested d))
    | Some (3, pk) ->
      Pbrt.Decoder.unexpected_payload_message "checker_start" 3 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : checker_start)

let rec decode_pb_checker_done d =
  let v = default_checker_done () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      ();
      continue__ := false
    | Some (1, Pbrt.Bytes) -> checker_done_set_prover v (Pbrt.Decoder.string d)
    | Some (1, pk) ->
      Pbrt.Decoder.unexpected_payload_message "checker_done" 1 pk
    | Some (2, Pbrt.Bytes) -> checker_done_set_checker v (Pbrt.Decoder.string d)
    | Some (2, pk) ->
      Pbrt.Decoder.unexpected_payload_message "checker_done" 2 pk
    | Some (3, Pbrt.Bytes) ->
      checker_done_set_problem v (decode_pb_problem (Pbrt.Decoder.nested d))
    | Some (3, pk) ->
      Pbrt.Decoder.unexpected_payload_message "checker_done" 3 pk
    | Some (4, Pbrt.Bytes) ->
      checker_done_set_proof_check_res v (Pbrt.Decoder.string d)
    | Some (4, pk) ->
      Pbrt.Decoder.unexpected_payload_message "checker_done" 4 pk
    | Some (5, Pbrt.Bytes) ->
      checker_done_set_raw v (decode_pb_run_proc_result (Pbrt.Decoder.nested d))
    | Some (5, pk) ->
      Pbrt.Decoder.unexpected_payload_message "checker_done" 5 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : checker_done)

let rec decode_pb_manifest d =
  let v = default_manifest () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      (* put lists in the correct order *)
      manifest_set_dirs v (List.rev v.dirs);
      manifest_set_provers v (List.rev v.provers);
      continue__ := false
    | Some (1, Pbrt.Bytes) -> manifest_set_uuid v (Pbrt.Decoder.string d)
    | Some (1, pk) -> Pbrt.Decoder.unexpected_payload_message "manifest" 1 pk
    | Some (2, Pbrt.Bytes) -> manifest_set_timestamp v (Pbrt.Decoder.string d)
    | Some (2, pk) -> Pbrt.Decoder.unexpected_payload_message "manifest" 2 pk
    | Some (3, Pbrt.Bytes) ->
      manifest_set_provers v (Pbrt.Decoder.string d :: v.provers)
    | Some (3, pk) -> Pbrt.Decoder.unexpected_payload_message "manifest" 3 pk
    | Some (4, Pbrt.Bits64) ->
      manifest_set_total_wall_time v (Pbrt.Decoder.float_as_bits64 d)
    | Some (4, pk) -> Pbrt.Decoder.unexpected_payload_message "manifest" 4 pk
    | Some (5, Pbrt.Varint) ->
      manifest_set_n_results v (Pbrt.Decoder.int32_as_varint d)
    | Some (5, pk) -> Pbrt.Decoder.unexpected_payload_message "manifest" 5 pk
    | Some (6, Pbrt.Varint) ->
      manifest_set_n_bad v (Pbrt.Decoder.int32_as_varint d)
    | Some (6, pk) -> Pbrt.Decoder.unexpected_payload_message "manifest" 6 pk
    | Some (7, Pbrt.Bytes) ->
      manifest_set_dirs v (Pbrt.Decoder.string d :: v.dirs)
    | Some (7, pk) -> Pbrt.Decoder.unexpected_payload_message "manifest" 7 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : manifest)

let rec decode_pb_event d =
  let rec loop () =
    let ret : event =
      match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "event"
      | Some (1, _) ->
        (Run_start (decode_pb_run_start (Pbrt.Decoder.nested d)) : event)
      | Some (2, _) ->
        (Run_done (decode_pb_run_done (Pbrt.Decoder.nested d)) : event)
      | Some (3, _) ->
        (Prover_start (decode_pb_prover_start (Pbrt.Decoder.nested d)) : event)
      | Some (4, _) ->
        (Prover_done (decode_pb_prover_done (Pbrt.Decoder.nested d)) : event)
      | Some (5, _) ->
        (Checker_start (decode_pb_checker_start (Pbrt.Decoder.nested d))
          : event)
      | Some (6, _) ->
        (Checker_done (decode_pb_checker_done (Pbrt.Decoder.nested d)) : event)
      | Some (n, payload_kind) ->
        Pbrt.Decoder.skip d payload_kind;
        loop ()
    in
    ret
  in
  loop ()

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf YoJson Encoding} *)

let rec encode_json_problem (v : problem) =
  let assoc = ref [] in
  if problem_has_name v then
    assoc := ("name", Pbrt_yojson.make_string v.name) :: !assoc;
  if problem_has_expected v then
    assoc := ("expected", Pbrt_yojson.make_string v.expected) :: !assoc;
  `Assoc !assoc

let rec encode_json_data_ref (v : data_ref) =
  match v with
  | Inline v -> `Assoc [ "inline", Pbrt_yojson.make_bytes v ]
  | Sha256ref v -> `Assoc [ "sha256ref", Pbrt_yojson.make_string v ]

let rec encode_json_run_proc_result (v : run_proc_result) =
  let assoc = ref [] in
  if run_proc_result_has_errcode v then
    assoc :=
      ("errcode", Pbrt_yojson.make_int (Int32.to_int v.errcode)) :: !assoc;
  (assoc :=
     match v.stdout with
     | None -> !assoc
     | Some v -> ("stdout", encode_json_data_ref v) :: !assoc);
  (assoc :=
     match v.stderr with
     | None -> !assoc
     | Some v -> ("stderr", encode_json_data_ref v) :: !assoc);
  if run_proc_result_has_rtime v then
    assoc :=
      ("rtime", Pbrt_yojson.make_string (string_of_float v.rtime)) :: !assoc;
  if run_proc_result_has_utime v then
    assoc :=
      ("utime", Pbrt_yojson.make_string (string_of_float v.utime)) :: !assoc;
  if run_proc_result_has_stime v then
    assoc :=
      ("stime", Pbrt_yojson.make_string (string_of_float v.stime)) :: !assoc;
  `Assoc !assoc

let rec encode_json_run_start (v : run_start) =
  let assoc = ref [] in
  if run_start_has_uuid v then
    assoc := ("uuid", Pbrt_yojson.make_string v.uuid) :: !assoc;
  if run_start_has_timestamp v then
    assoc :=
      ("timestamp", Pbrt_yojson.make_string (string_of_float v.timestamp))
      :: !assoc;
  (assoc :=
     let l = v.dirs |> List.map Pbrt_yojson.make_string in
     ("dirs", `List l) :: !assoc);
  (assoc :=
     let l = v.provers |> List.map Pbrt_yojson.make_string in
     ("provers", `List l) :: !assoc);
  `Assoc !assoc

let rec encode_json_run_done (v : run_done) =
  let assoc = ref [] in
  if run_done_has_total_wall_time v then
    assoc :=
      ( "totalWallTime",
        Pbrt_yojson.make_string (string_of_float v.total_wall_time) )
      :: !assoc;
  if run_done_has_n_results v then
    assoc :=
      ("nResults", Pbrt_yojson.make_int (Int32.to_int v.n_results)) :: !assoc;
  if run_done_has_n_bad v then
    assoc := ("nBad", Pbrt_yojson.make_int (Int32.to_int v.n_bad)) :: !assoc;
  `Assoc !assoc

let rec encode_json_prover_start (v : prover_start) =
  let assoc = ref [] in
  if prover_start_has_prover v then
    assoc := ("prover", Pbrt_yojson.make_string v.prover) :: !assoc;
  (assoc :=
     match v.problem with
     | None -> !assoc
     | Some v -> ("problem", encode_json_problem v) :: !assoc);
  if prover_start_has_timeout_s v then
    assoc :=
      ("timeoutS", Pbrt_yojson.make_string (Int64.to_string v.timeout_s))
      :: !assoc;
  `Assoc !assoc

let rec encode_json_prover_done (v : prover_done) =
  let assoc = ref [] in
  if prover_done_has_prover v then
    assoc := ("prover", Pbrt_yojson.make_string v.prover) :: !assoc;
  (assoc :=
     match v.problem with
     | None -> !assoc
     | Some v -> ("problem", encode_json_problem v) :: !assoc);
  if prover_done_has_res v then
    assoc := ("res", Pbrt_yojson.make_string v.res) :: !assoc;
  (assoc :=
     let l = v.labels |> List.map Pbrt_yojson.make_string in
     ("labels", `List l) :: !assoc);
  if prover_done_has_timeout_s v then
    assoc :=
      ("timeoutS", Pbrt_yojson.make_string (Int64.to_string v.timeout_s))
      :: !assoc;
  (assoc :=
     match v.raw with
     | None -> !assoc
     | Some v -> ("raw", encode_json_run_proc_result v) :: !assoc);
  `Assoc !assoc

let rec encode_json_checker_start (v : checker_start) =
  let assoc = ref [] in
  if checker_start_has_prover v then
    assoc := ("prover", Pbrt_yojson.make_string v.prover) :: !assoc;
  if checker_start_has_checker v then
    assoc := ("checker", Pbrt_yojson.make_string v.checker) :: !assoc;
  (assoc :=
     match v.problem with
     | None -> !assoc
     | Some v -> ("problem", encode_json_problem v) :: !assoc);
  `Assoc !assoc

let rec encode_json_checker_done (v : checker_done) =
  let assoc = ref [] in
  if checker_done_has_prover v then
    assoc := ("prover", Pbrt_yojson.make_string v.prover) :: !assoc;
  if checker_done_has_checker v then
    assoc := ("checker", Pbrt_yojson.make_string v.checker) :: !assoc;
  (assoc :=
     match v.problem with
     | None -> !assoc
     | Some v -> ("problem", encode_json_problem v) :: !assoc);
  if checker_done_has_proof_check_res v then
    assoc :=
      ("proofCheckRes", Pbrt_yojson.make_string v.proof_check_res) :: !assoc;
  (assoc :=
     match v.raw with
     | None -> !assoc
     | Some v -> ("raw", encode_json_run_proc_result v) :: !assoc);
  `Assoc !assoc

let rec encode_json_manifest (v : manifest) =
  let assoc = ref [] in
  if manifest_has_uuid v then
    assoc := ("uuid", Pbrt_yojson.make_string v.uuid) :: !assoc;
  if manifest_has_timestamp v then
    assoc := ("timestamp", Pbrt_yojson.make_string v.timestamp) :: !assoc;
  (assoc :=
     let l = v.provers |> List.map Pbrt_yojson.make_string in
     ("provers", `List l) :: !assoc);
  if manifest_has_total_wall_time v then
    assoc :=
      ( "totalWallTime",
        Pbrt_yojson.make_string (string_of_float v.total_wall_time) )
      :: !assoc;
  if manifest_has_n_results v then
    assoc :=
      ("nResults", Pbrt_yojson.make_int (Int32.to_int v.n_results)) :: !assoc;
  if manifest_has_n_bad v then
    assoc := ("nBad", Pbrt_yojson.make_int (Int32.to_int v.n_bad)) :: !assoc;
  (assoc :=
     let l = v.dirs |> List.map Pbrt_yojson.make_string in
     ("dirs", `List l) :: !assoc);
  `Assoc !assoc

let rec encode_json_event (v : event) =
  match v with
  | Run_start v -> `Assoc [ "runStart", encode_json_run_start v ]
  | Run_done v -> `Assoc [ "runDone", encode_json_run_done v ]
  | Prover_start v -> `Assoc [ "proverStart", encode_json_prover_start v ]
  | Prover_done v -> `Assoc [ "proverDone", encode_json_prover_done v ]
  | Checker_start v -> `Assoc [ "checkerStart", encode_json_checker_start v ]
  | Checker_done v -> `Assoc [ "checkerDone", encode_json_checker_done v ]

[@@@ocaml.warning "-23-27-30-39"]

(** {2 JSON Decoding} *)

let rec decode_json_problem d =
  let v = default_problem () in
  let assoc =
    match d with
    | `Assoc assoc -> assoc
    | _ -> assert false
  in
  List.iter
    (function
      | "name", json_value ->
        problem_set_name v (Pbrt_yojson.string json_value "problem" "name")
      | "expected", json_value ->
        problem_set_expected v
          (Pbrt_yojson.string json_value "problem" "expected")
      | _, _ -> () (*Unknown fields are ignored*))
    assoc;
  ({ _presence = v._presence; name = v.name; expected = v.expected } : problem)

let rec decode_json_data_ref json =
  let assoc =
    match json with
    | `Assoc assoc -> assoc
    | _ -> assert false
  in
  let rec loop = function
    | [] -> Pbrt_yojson.E.malformed_variant "data_ref"
    | ("inline", json_value) :: _ ->
      (Inline (Pbrt_yojson.bytes json_value "data_ref" "Inline") : data_ref)
    | ("sha256ref", json_value) :: _ ->
      (Sha256ref (Pbrt_yojson.string json_value "data_ref" "Sha256ref")
        : data_ref)
    | _ :: tl -> loop tl
  in
  loop assoc

let rec decode_json_run_proc_result d =
  let v = default_run_proc_result () in
  let assoc =
    match d with
    | `Assoc assoc -> assoc
    | _ -> assert false
  in
  List.iter
    (function
      | "errcode", json_value ->
        run_proc_result_set_errcode v
          (Pbrt_yojson.int32 json_value "run_proc_result" "errcode")
      | "stdout", json_value ->
        run_proc_result_set_stdout v (decode_json_data_ref json_value)
      | "stderr", json_value ->
        run_proc_result_set_stderr v (decode_json_data_ref json_value)
      | "rtime", json_value ->
        run_proc_result_set_rtime v
          (Pbrt_yojson.float json_value "run_proc_result" "rtime")
      | "utime", json_value ->
        run_proc_result_set_utime v
          (Pbrt_yojson.float json_value "run_proc_result" "utime")
      | "stime", json_value ->
        run_proc_result_set_stime v
          (Pbrt_yojson.float json_value "run_proc_result" "stime")
      | _, _ -> () (*Unknown fields are ignored*))
    assoc;
  ({
     _presence = v._presence;
     errcode = v.errcode;
     stdout = v.stdout;
     stderr = v.stderr;
     rtime = v.rtime;
     utime = v.utime;
     stime = v.stime;
   }
    : run_proc_result)

let rec decode_json_run_start d =
  let v = default_run_start () in
  let assoc =
    match d with
    | `Assoc assoc -> assoc
    | _ -> assert false
  in
  List.iter
    (function
      | "uuid", json_value ->
        run_start_set_uuid v (Pbrt_yojson.string json_value "run_start" "uuid")
      | "timestamp", json_value ->
        run_start_set_timestamp v
          (Pbrt_yojson.float json_value "run_start" "timestamp")
      | "dirs", `List l ->
        run_start_set_dirs v
        @@ List.map
             (function
               | json_value -> Pbrt_yojson.string json_value "run_start" "dirs")
             l
      | "provers", `List l ->
        run_start_set_provers v
        @@ List.map
             (function
               | json_value ->
                 Pbrt_yojson.string json_value "run_start" "provers")
             l
      | _, _ -> () (*Unknown fields are ignored*))
    assoc;
  ({
     _presence = v._presence;
     uuid = v.uuid;
     timestamp = v.timestamp;
     dirs = v.dirs;
     provers = v.provers;
   }
    : run_start)

let rec decode_json_run_done d =
  let v = default_run_done () in
  let assoc =
    match d with
    | `Assoc assoc -> assoc
    | _ -> assert false
  in
  List.iter
    (function
      | "totalWallTime", json_value ->
        run_done_set_total_wall_time v
          (Pbrt_yojson.float json_value "run_done" "total_wall_time")
      | "nResults", json_value ->
        run_done_set_n_results v
          (Pbrt_yojson.int32 json_value "run_done" "n_results")
      | "nBad", json_value ->
        run_done_set_n_bad v (Pbrt_yojson.int32 json_value "run_done" "n_bad")
      | _, _ -> () (*Unknown fields are ignored*))
    assoc;
  ({
     _presence = v._presence;
     total_wall_time = v.total_wall_time;
     n_results = v.n_results;
     n_bad = v.n_bad;
   }
    : run_done)

let rec decode_json_prover_start d =
  let v = default_prover_start () in
  let assoc =
    match d with
    | `Assoc assoc -> assoc
    | _ -> assert false
  in
  List.iter
    (function
      | "prover", json_value ->
        prover_start_set_prover v
          (Pbrt_yojson.string json_value "prover_start" "prover")
      | "problem", json_value ->
        prover_start_set_problem v (decode_json_problem json_value)
      | "timeoutS", json_value ->
        prover_start_set_timeout_s v
          (Pbrt_yojson.int64 json_value "prover_start" "timeout_s")
      | _, _ -> () (*Unknown fields are ignored*))
    assoc;
  ({
     _presence = v._presence;
     prover = v.prover;
     problem = v.problem;
     timeout_s = v.timeout_s;
   }
    : prover_start)

let rec decode_json_prover_done d =
  let v = default_prover_done () in
  let assoc =
    match d with
    | `Assoc assoc -> assoc
    | _ -> assert false
  in
  List.iter
    (function
      | "prover", json_value ->
        prover_done_set_prover v
          (Pbrt_yojson.string json_value "prover_done" "prover")
      | "problem", json_value ->
        prover_done_set_problem v (decode_json_problem json_value)
      | "res", json_value ->
        prover_done_set_res v
          (Pbrt_yojson.string json_value "prover_done" "res")
      | "labels", `List l ->
        prover_done_set_labels v
        @@ List.map
             (function
               | json_value ->
                 Pbrt_yojson.string json_value "prover_done" "labels")
             l
      | "timeoutS", json_value ->
        prover_done_set_timeout_s v
          (Pbrt_yojson.int64 json_value "prover_done" "timeout_s")
      | "raw", json_value ->
        prover_done_set_raw v (decode_json_run_proc_result json_value)
      | _, _ -> () (*Unknown fields are ignored*))
    assoc;
  ({
     _presence = v._presence;
     prover = v.prover;
     problem = v.problem;
     res = v.res;
     labels = v.labels;
     timeout_s = v.timeout_s;
     raw = v.raw;
   }
    : prover_done)

let rec decode_json_checker_start d =
  let v = default_checker_start () in
  let assoc =
    match d with
    | `Assoc assoc -> assoc
    | _ -> assert false
  in
  List.iter
    (function
      | "prover", json_value ->
        checker_start_set_prover v
          (Pbrt_yojson.string json_value "checker_start" "prover")
      | "checker", json_value ->
        checker_start_set_checker v
          (Pbrt_yojson.string json_value "checker_start" "checker")
      | "problem", json_value ->
        checker_start_set_problem v (decode_json_problem json_value)
      | _, _ -> () (*Unknown fields are ignored*))
    assoc;
  ({
     _presence = v._presence;
     prover = v.prover;
     checker = v.checker;
     problem = v.problem;
   }
    : checker_start)

let rec decode_json_checker_done d =
  let v = default_checker_done () in
  let assoc =
    match d with
    | `Assoc assoc -> assoc
    | _ -> assert false
  in
  List.iter
    (function
      | "prover", json_value ->
        checker_done_set_prover v
          (Pbrt_yojson.string json_value "checker_done" "prover")
      | "checker", json_value ->
        checker_done_set_checker v
          (Pbrt_yojson.string json_value "checker_done" "checker")
      | "problem", json_value ->
        checker_done_set_problem v (decode_json_problem json_value)
      | "proofCheckRes", json_value ->
        checker_done_set_proof_check_res v
          (Pbrt_yojson.string json_value "checker_done" "proof_check_res")
      | "raw", json_value ->
        checker_done_set_raw v (decode_json_run_proc_result json_value)
      | _, _ -> () (*Unknown fields are ignored*))
    assoc;
  ({
     _presence = v._presence;
     prover = v.prover;
     checker = v.checker;
     problem = v.problem;
     proof_check_res = v.proof_check_res;
     raw = v.raw;
   }
    : checker_done)

let rec decode_json_manifest d =
  let v = default_manifest () in
  let assoc =
    match d with
    | `Assoc assoc -> assoc
    | _ -> assert false
  in
  List.iter
    (function
      | "uuid", json_value ->
        manifest_set_uuid v (Pbrt_yojson.string json_value "manifest" "uuid")
      | "timestamp", json_value ->
        manifest_set_timestamp v
          (Pbrt_yojson.string json_value "manifest" "timestamp")
      | "provers", `List l ->
        manifest_set_provers v
        @@ List.map
             (function
               | json_value ->
                 Pbrt_yojson.string json_value "manifest" "provers")
             l
      | "totalWallTime", json_value ->
        manifest_set_total_wall_time v
          (Pbrt_yojson.float json_value "manifest" "total_wall_time")
      | "nResults", json_value ->
        manifest_set_n_results v
          (Pbrt_yojson.int32 json_value "manifest" "n_results")
      | "nBad", json_value ->
        manifest_set_n_bad v (Pbrt_yojson.int32 json_value "manifest" "n_bad")
      | "dirs", `List l ->
        manifest_set_dirs v
        @@ List.map
             (function
               | json_value -> Pbrt_yojson.string json_value "manifest" "dirs")
             l
      | _, _ -> () (*Unknown fields are ignored*))
    assoc;
  ({
     _presence = v._presence;
     uuid = v.uuid;
     timestamp = v.timestamp;
     provers = v.provers;
     total_wall_time = v.total_wall_time;
     n_results = v.n_results;
     n_bad = v.n_bad;
     dirs = v.dirs;
   }
    : manifest)

let rec decode_json_event json =
  let assoc =
    match json with
    | `Assoc assoc -> assoc
    | _ -> assert false
  in
  let rec loop = function
    | [] -> Pbrt_yojson.E.malformed_variant "event"
    | ("runStart", json_value) :: _ ->
      (Run_start (decode_json_run_start json_value) : event)
    | ("runDone", json_value) :: _ ->
      (Run_done (decode_json_run_done json_value) : event)
    | ("proverStart", json_value) :: _ ->
      (Prover_start (decode_json_prover_start json_value) : event)
    | ("proverDone", json_value) :: _ ->
      (Prover_done (decode_json_prover_done json_value) : event)
    | ("checkerStart", json_value) :: _ ->
      (Checker_start (decode_json_checker_start json_value) : event)
    | ("checkerDone", json_value) :: _ ->
      (Checker_done (decode_json_checker_done json_value) : event)
    | _ :: tl -> loop tl
  in
  loop assoc

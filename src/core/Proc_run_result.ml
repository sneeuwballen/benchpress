
type t = {
  (* Raw output *)
  errcode: int;
  stdout: string;
  stderr: string;

  (* Time used *)
  rtime : float;
  utime : float;
  stime : float;
}

let pp out (r:t): unit =
  Format.fprintf out
    "(@[:errcode %d@ rtime %.2f@ :utime %.2f@ :stime %.2f@ \
     :stdout %S@ :stderr %S@])"
    r.errcode r.rtime r.utime r.stime r.stdout r.stderr

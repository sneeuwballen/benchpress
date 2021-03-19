
let spf = Printf.sprintf

type span = float

let out_ : out_channel CCLock.t option ref = ref None

let[@inline] enabled () = !out_ != None
let start_ = Unix.gettimeofday()
let[@inline] now_us () = (Unix.gettimeofday () -. start_) *. 1e6

let enable () =
  (* open subprocess to write into [trace.json.gz] *)
  if not (enabled()) then (
    Logs.info (fun k->k"enable profiling");
    (* TODO *)
    (*     let oc = Unix.open_process_out "gzip --synchronous -c - > trace.json.gz" in *)
    let oc = open_out "trace.json" in
    output_string oc "[";
    let out = CCLock.create oc in
    out_ := Some out;
    at_exit (fun () ->
        out_ := None;
        CCLock.with_lock out (fun oc -> flush oc; close_out_noerr oc));
  )

let[@inline] enter () : span =
  if enabled() then now_us () else 0.

let exit_real_ ?(args=[]) sp name =
  match !out_ with
  | None -> ()
  | Some out ->
    let args = List.map (fun (k,v) -> spf",%S:%S" k v) args |> String.concat ","in
    let dur = (now_us () -. sp) in
    let tid = Thread.id (Thread.self()) in
    let s = spf
        {json|{"ph":"X","name":%S,"pid":1,"tid":%d,"ts":%.1f,"dur":%.1f%s},|json}
        name tid sp dur args in
    CCLock.with_lock out
      (fun oc -> output_string oc s; output_char oc '\n'; flush oc)

let[@inline] exit ?args (name:string) (sp:span) : unit =
  if sp>0. then exit_real_ ?args sp name

let[@inline] with_ ?args name f =
  if enabled() then (
    let sp = enter() in
    CCFun.finally ~h:(fun () -> exit ?args name sp) ~f
  ) else f ()

let[@inline] with1 ?args name f x =
  if enabled() then (
    let sp = enter() in
    CCFun.finally1 ~h:(fun () -> exit ?args name sp) f x
  ) else f x

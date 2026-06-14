open Common

let show_lvl = function
  | Logs.Debug -> "<7>DEBUG"
  | Logs.Info -> "<6>INFO"
  | Logs.Error -> "<3>ERROR"
  | Logs.Warning -> "<4>WARNING"
  | Logs.App -> "<5>APP"

let make_stdout () : Logs.reporter =
  let app = Format.std_formatter in
  let dst = Format.std_formatter in
  let pp_header out (lvl, src) : unit =
    let src =
      match src with
      | None -> ""
      | Some s -> spf "[%s]" s
    in
    Fmt.fprintf out "%s%s: " (show_lvl lvl) src
  in
  Logs.format_reporter ~pp_header ~app ~dst ()

let setup (lvl : Logs.level option) =
  let m = Mutex.create () in
  Logs.set_reporter_mutex
    ~lock:(fun () -> Mutex.lock m)
    ~unlock:(fun () -> Mutex.unlock m);
  Logs.set_level ~all:true lvl;
  Logs.set_reporter @@ make_stdout ()

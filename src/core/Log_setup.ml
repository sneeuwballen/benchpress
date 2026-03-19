(* This file is free software. See file "license" for more details. *)

(** Logging setup *)

module Log_report = struct
  let buf_fmt () =
    let b = Buffer.create 512 in
    let fmt = CCFormat.formatter_of_buffer b in
    CCFormat.set_color_tag_handling fmt;
    let flush () =
      CCFormat.fprintf fmt "@]@?";
      let m = Buffer.contents b in
      Buffer.reset b;
      m
    in
    fmt, flush

  let pp_h out (src, lvl, _) =
    let src = Logs.Src.name src in
    let src =
      if src = "application" then
        ""
      else
        src
    in
    match lvl with
    | Logs.Debug -> CCFormat.fprintf out "[@{<black>debug@}:%s]" src
    | Logs.Info -> CCFormat.fprintf out "[@{<cyan>info@}:%s]" src
    | Logs.Error -> CCFormat.fprintf out "[@{<Red>error@}:%s]" src
    | Logs.Warning -> CCFormat.fprintf out "[@{<yellow>warning@}:%s]" src
    | Logs.App -> CCFormat.fprintf out "[@{<blue>app@}:%s]" src

  let reporter () =
    CCFormat.set_color_default true;
    let buf_out, buf_flush = buf_fmt () in
    let pp_header fmt header =
      let now = Ptime_clock.now () |> Ptime.to_float_s in
      CCFormat.fprintf fmt "@[<2>[%a|t%d] %a@ " ISO8601.Permissive.pp_datetime now
        Thread.(id @@ self ())
        pp_h header
    in
    let out, err =
      match Sys.getenv "LOGS_FILE" with
      | "" -> stdout, stderr
      | file ->
        (try
           let oc = open_out file in
           at_exit (fun () -> close_out_noerr oc);
           oc, oc
         with e ->
           Printf.eprintf "error: cannot open log file '%s': %s\n%!" file
             (Printexc.to_string e);
           stdout, stderr)
      | exception Not_found -> stdout, stderr
    in
    let report src level ~over k msgf =
      let k _ =
        let reset_line = "\x1b[2K\r" in
        let write_str out s =
          Moonpool.Lock.with_ (Moonpool.Lock.create ()) @@ fun () ->
          Printf.fprintf out "%s%s%!" reset_line s
        in
        let msg = buf_flush () in
        write_str
          (match level with
          | Logs.App -> out
          | _ -> err)
          msg;
        over ();
        k ()
      in
      msgf (fun ?header ?tags:_ fmt ->
          CCFormat.kfprintf k buf_out
            ("%a@[" ^^ fmt ^^ "@]@.")
            pp_header (src, level, header))
    in
    { Logs.report }
end

(** Setup the logging infra *)
let setup_logs (lvl : Logs.level option) : unit =
  Logs.set_reporter (Log_report.reporter ());
  Logs.set_level ~all:true lvl;
  (let module Log = (val Logs.src_log (Logs.Src.create "benchpress.setup")) in
   Log.debug (fun k -> k "logs are setup"));
  ()

(* This file is free software. See file "license" for more details. *)

(** Logging setup *)

module Logfmt = struct
  let escape_value s =
    if
      CCString.exists
        (fun c ->
          Char.code c < 0x2f
          || Char.code c >= 127
          || c = ' ' || c = '\\' || c = '"')
        s
    then
      Printf.sprintf "%S" s
    else
      s

  let add_kv_to_buffer buf k v =
    Buffer.add_string buf k;
    Buffer.add_char buf '=';
    Buffer.add_string buf (escape_value v)

  let to_buffer buf l =
    List.iteri
      (fun i (k, v) ->
        if i > 0 then Buffer.add_char buf ' ';
        add_kv_to_buffer buf k v)
      l
end

module Log_report = struct
  type output = { oc: out_channel; must_close: bool }

  let get_output () : output =
    match Sys.getenv_opt "LOGFILE" with
    | Some file ->
      let oc = open_out file in
      { oc; must_close = true }
    | None -> { oc = stderr; must_close = false }

  let reporter () =
    let out = get_output () in
    if out.must_close then
      at_exit (fun () ->
          flush out.oc;
          close_out_noerr out.oc);

    let buf = Buffer.create 32 in
    let buf_fmt = Format.formatter_of_buffer buf in
    let buf_lock = Mutex.create () in

    let write_str s =
      Global_lock.synchronized (fun () ->
          output_string out.oc s;
          flush out.oc)
    in

    let report src level ~over k msgf =
      let k fmt =
        Format.pp_print_flush fmt ();
        (* the log message *)
        let msg = Buffer.contents buf in
        Buffer.clear buf;

        (* key values common to all lines *)
        let common =
          [
            "time", Ptime.to_rfc3339 ~space:false (Ptime_clock.now ());
            "level", Logs.level_to_string (Some level);
            "src", Logs.Src.name src;
          ]
        in

        (* emit one log line per line in the message *)
        let lines = String.split_on_char '\n' msg in
        List.iter
          (fun line ->
            Buffer.clear buf;
            Logfmt.to_buffer buf (common @ [ "msg", line ]);
            Buffer.add_char buf '\n')
          lines;

        (* get content, we can then unlock buffer *)
        let log_data = Buffer.contents buf in
        Buffer.clear buf;
        Mutex.unlock buf_lock;

        write_str log_data;
        over ();
        k ()
      in

      Mutex.lock buf_lock;
      msgf (fun ?header:_ ?tags:_ fmt -> CCFormat.kfprintf k buf_fmt fmt)
    in
    { Logs.report }

  (*
    let pp_header fmt header =
      let now = Ptime_clock.now () in
      CCFormat.fprintf fmt "@[<2>[%a|t%d] %a@ "
        (Ptime.pp_rfc3339 ~frac_s:3 ())
        now
        Thread.(id @@ self ())
        pp_h header
    in
    let log_out =
      match Sys.getenv "LOGS_FILE" with
      | "" -> stderr
      | file ->
        (try
           let oc = open_out file in
           at_exit (fun () -> close_out_noerr oc);
           oc
         with e ->
           Printf.eprintf "error: cannot open log file '%s': %s\n%!" file
             (Printexc.to_string e);
           stderr)
      | exception Not_found -> stderr
    in
    let log_mutex = Mutex.create () in
    let report src level ~over k msgf =
      let k _ =
        let reset_line = "\x1b[2K\r" in
        let write_str s =
          Mutex.lock log_mutex;
          (try Printf.fprintf log_out "%s%s%!" reset_line s with _ -> ());
          Mutex.unlock log_mutex
        in
        let msg = buf_flush () in
        write_str msg;
        over ();
        k ()
      in
      msgf (fun ?header ?tags:_ fmt ->
          CCFormat.kfprintf k buf_out
            ("%a@[" ^^ fmt ^^ "@]@.")
            pp_header (src, level, header))
    in
    { Logs.report }
    *)
end

(** Setup the logging infra *)
let setup_logs (lvl : Logs.level option) : unit =
  Logs.set_reporter (Log_report.reporter ());
  Logs.set_level ~all:true lvl;
  (let module Log = (val Logs.src_log (Logs.Src.create "benchpress.setup")) in
  Log.debug (fun k -> k "logs are setup"));
  ()

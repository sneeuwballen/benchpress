(* This file is free software. See file "license" for more details. *)

(** Logging setup *)

open struct
  let ( let@ ) = ( @@ )
end

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

  let buf_fmt () =
    let b = Buffer.create 512 in
    let out = CCFormat.formatter_of_buffer b in
    let flush () =
      Format.pp_print_flush out ();
      let m = Buffer.contents b in
      Buffer.reset b;
      m
    in
    out, b, flush

  let reporter () =
    let out = get_output () in
    if out.must_close then
      at_exit (fun () ->
          Global_lock.synchronized (fun () ->
              flush out.oc;
              close_out_noerr out.oc));

    let buf_pool =
      Apool.create ~clear:Buffer.reset ~mk_item:(fun () -> Buffer.create 32) ()
    in
    let buf_fmt_pool =
      Apool.create
        ~clear:(fun (_, b, _) -> Buffer.reset b)
        ~mk_item:buf_fmt ~max_size:16 ()
    in

    let write_str s =
      Global_lock.synchronized (fun () ->
          output_string out.oc s;
          flush out.oc)
    in

    let report src level ~over k msgf =
      let k flush _fmt =
        (* the log message *)
        let msg = flush () in

        (* key values common to all lines *)
        let common =
          [
            "time", Ptime.to_rfc3339 ~frac_s:3 ~space:false (Ptime_clock.now ());
            "level", Logs.level_to_string (Some level);
            "src", Logs.Src.name src;
          ]
        in

        (* emit one log line per line in the message *)
        let log_data =
          let@ buf = Apool.with_resource buf_pool in
          let lines = String.split_on_char '\n' msg in
          List.iter
            (fun line ->
              let line = String.trim line in
              if line <> "" then (
                Logfmt.to_buffer buf (common @ [ "msg", line ]);
                Buffer.add_char buf '\n'
              ))
            lines;

          (* get content, we can then unlock buffer *)
          Buffer.contents buf
        in

        write_str log_data;
        over ();
        k ()
      in

      msgf (fun ?header:_ ?tags:_ fmt ->
          let@ buf_fmt, _, flush = Apool.with_resource buf_fmt_pool in
          CCFormat.kfprintf (k flush) buf_fmt fmt)
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

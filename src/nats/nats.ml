(** Minimal NATS client using Eio.

    The NATS protocol is a text-based line protocol. Lines end with CRLF.
    Messages have a header line with byte counts, followed by the payload and a
    trailing CRLF. Supports both [MSG] and [HMSG] (headers). *)

let crlf = "\r\n"
let hdr_line = "NATS/1.0\r\n"
let ( let@ ) = ( @@ )
let spf = Printf.sprintf

let is_bad_char = function
  | '.' | ' ' -> true
  | _ -> false

let is_bad_char_strict = function
  | '.' | ' ' | '>' | '*' -> true
  | _ -> false

let make_subject ~is_bad = function
  | [] -> invalid_arg "subject must have at least one component"
  | parts ->
    List.iter
      (fun s ->
        if s = "" then invalid_arg "subject component must not be empty";
        String.iter
          (fun c ->
            if is_bad c then
              invalid_arg
                (spf "subject component %S contains invalid char '%c'" s c))
          s)
      parts;
    String.concat "." parts

let subject_of_list = make_subject ~is_bad:is_bad_char_strict
let subject_of_list_for_sub = make_subject ~is_bad:is_bad_char

module Log = (val Logs.src_log (Logs.Src.create "benchpress.nats"))

module Int_tbl = Hashtbl.Make (struct
  type t = int

  let equal = ( = )
  let hash = Hashtbl.hash
end)

(** Build a random blob that uniquely identify this client *)
let make_inbox_prefix () =
  let st = Random.State.make_self_init () in
  let bits () = Random.State.bits st in
  spf "%08x%08x%08x%08x%08x" (bits ()) (bits ()) (bits ()) (bits ()) (bits ())

(** {2 Connection state} *)

type header = string * string

type msg = {
  subject: string list;
  sid: int;
  reply_to: string option;
  headers: header list option;
  payload: string;
}

type sub_data = { f: msg -> unit } [@@unboxed]

type t = {
  flow: Eio.Flow.sink_ty Eio.Flow.sink;
  write_mutex: Eio.Mutex.t;
  sw: Eio.Switch.t;
  subs: sub_data Int_tbl.t;
  subs_mutex: Eio.Mutex.t;
  next_sid: int Atomic.t;
  is_done: unit Eio.Promise.t;
  shutdown: unit -> unit;
  inbox_prefix: string;
  inbox_counter: int Atomic.t;
  inbox_promises: msg Eio.Promise.u Int_tbl.t;
  inbox_promises_mutex: Eio.Mutex.t;
}

type sub = int

module Proto = struct
  let write_str self s =
    Eio.Mutex.use_rw ~protect:true self.write_mutex (fun () ->
        try Eio.Flow.copy_string s self.flow with _ -> ())

  let send_connect t ?token ?user ?pass () =
    let buf = Buffer.create 64 in
    Buffer.add_string buf
      {|CONNECT {"verbose":false,"pedantic":false,"headers":true|};
    Option.iter (fun t -> Printf.bprintf buf {|,"auth_token":"%s"|} t) token;
    Option.iter (fun u -> Printf.bprintf buf {|,"user":"%s"|} u) user;
    Option.iter (fun p -> Printf.bprintf buf {|,"pass":"%s"|} p) pass;
    Buffer.add_string buf "}";
    Buffer.add_string buf crlf;
    write_str t (Buffer.contents buf)

  let send_pong t = write_str t ("PONG" ^ crlf)

  let send_sub t ~sid subject queue =
    match queue with
    | None -> write_str t (spf "SUB %s %d%s" subject sid crlf)
    | Some q -> write_str t (spf "SUB %s %s %d%s" subject q sid crlf)

  let send_unsub t ~sid ~max_msgs =
    match max_msgs with
    | None -> write_str t (spf "UNSUB %d%s" sid crlf)
    | Some n -> write_str t (spf "UNSUB %d %d%s" sid n crlf)

  let send_pub t ~subject ~reply_to ~payload =
    let n = String.length payload in
    (match reply_to with
    | None -> write_str t (spf "PUB %s %d%s" subject n crlf)
    | Some rt -> write_str t (spf "PUB %s %s %d%s" subject rt n crlf));
    write_str t payload;
    write_str t crlf

  let encode_headers hs =
    String.concat "" (List.map (fun (k, v) -> k ^ ": " ^ v ^ crlf) hs)

  let send_hpub t ~subject ~reply_to ~headers ~payload =
    let hdr = hdr_line ^ encode_headers headers ^ crlf in
    let hdr_len = String.length hdr in
    let total_len = hdr_len + String.length payload in
    (match reply_to with
    | None -> write_str t (spf "HPUB %s %d %d%s" subject hdr_len total_len crlf)
    | Some rt ->
      write_str t (spf "HPUB %s %s %d %d%s" subject rt hdr_len total_len crlf));
    write_str t hdr;
    write_str t payload;
    write_str t crlf

  let check_crlf msg =
    if msg <> crlf then failwith (spf "expected %S but got %S" crlf msg)

  let parse_header line =
    match String.index_opt line ':' with
    | Some i ->
      let k = String.trim (String.sub line 0 i) in
      let v =
        String.trim (String.sub line (i + 1) (String.length line - i - 1))
      in
      k, v
    | None -> failwith (spf "invalid header: %S" line)

  let parse_headers str =
    String.split_on_char '\n' str
    |> List.filter_map (fun line ->
           let line = String.trim line in
           if line = "" then
             None
           else
             Some (parse_header line))

  let read_hmsg buf subject sid reply_to hdr_len total_len =
    let hdr_data = Eio.Buf_read.take hdr_len buf in
    let payload_len = total_len - hdr_len in
    let payload = Eio.Buf_read.take payload_len buf in
    check_crlf (Eio.Buf_read.take 2 buf);
    let headers =
      let hdr_line_len = String.length hdr_line in
      if hdr_len > hdr_line_len + 2 then (
        let raw =
          String.sub hdr_data hdr_line_len (hdr_len - hdr_line_len - 2)
        in
        let hs = parse_headers raw in
        if hs = [] then
          None
        else
          Some hs
      ) else
        None
    in
    {
      subject = String.split_on_char '.' subject;
      sid;
      reply_to;
      headers;
      payload;
    }

  let parse_msg line buf =
    let parts =
      String.split_on_char ' ' line |> List.filter (fun s -> s <> "")
    in
    match parts with
    | [ "MSG"; subject; sid_s; size_s ] ->
      let payload = Eio.Buf_read.take (int_of_string size_s) buf in
      check_crlf (Eio.Buf_read.take 2 buf);
      {
        subject = String.split_on_char '.' subject;
        sid = int_of_string sid_s;
        reply_to = None;
        headers = None;
        payload;
      }
    | [ "MSG"; subject; sid_s; reply_to; size_s ] ->
      let payload = Eio.Buf_read.take (int_of_string size_s) buf in
      check_crlf (Eio.Buf_read.take 2 buf);
      {
        subject = String.split_on_char '.' subject;
        sid = int_of_string sid_s;
        reply_to = Some reply_to;
        headers = None;
        payload;
      }
    | [ "HMSG"; subject; sid_s; hdr_len_s; total_len_s ] ->
      let sid = int_of_string sid_s in
      let hdr_len = int_of_string hdr_len_s in
      let total_len = int_of_string total_len_s in
      read_hmsg buf subject sid None hdr_len total_len
    | [ "HMSG"; subject; sid_s; reply_to; hdr_len_s; total_len_s ] ->
      let sid = int_of_string sid_s in
      let hdr_len = int_of_string hdr_len_s in
      let total_len = int_of_string total_len_s in
      read_hmsg buf subject sid (Some reply_to) hdr_len total_len
    | _ -> failwith (spf "bad protocol line: %S" line)
end

open Proto

(** Handle replies for our requests. Returns [true] if this was actually a reply
    we were waiting for, [false] otherwise. *)
let dispatch_inbox_reply self msg : bool =
  match msg.subject with
  | [ "_INBOX"; _prefix; counter_s ] when _prefix = self.inbox_prefix ->
    let counter = int_of_string counter_s in
    let resolver_opt =
      let@ () = Eio.Mutex.use_rw ~protect:true self.inbox_promises_mutex in
      match Int_tbl.find_opt self.inbox_promises counter with
      | Some resolver ->
        Int_tbl.remove self.inbox_promises counter;
        Some resolver
      | None -> None
    in
    (match resolver_opt with
    | None -> false
    | Some r ->
      Eio.Promise.resolve r msg;
      true)
  | _ -> false

(** Dispatch received [msg] to subscribers *)
let dispatch_msg self msg : unit =
  let dispatched_reply = dispatch_inbox_reply self msg in
  if not dispatched_reply then (
    let f_opt =
      let@ () = Eio.Mutex.use_rw ~protect:true self.subs_mutex in
      match Int_tbl.find_opt self.subs msg.sid with
      | None -> None
      | Some s -> Some s.f
    in
    Option.iter
      (fun f ->
        (* run [f] in a fiber *)
        let@ () = Eio.Fiber.fork ~sw:self.sw in
        try f msg
        with exn ->
          Log.warn (fun k ->
              k "callback for sub on %s raised: %s"
                (String.concat "." msg.subject)
                (Printexc.to_string exn)))
      f_opt
  )

(** {2 Reader loop} *)

let rec reader_loop t buf =
  match Eio.Buf_read.line buf with
  | "PING" ->
    send_pong t;
    reader_loop t buf
  | "PONG" | "+OK" -> reader_loop t buf
  | line when String.starts_with ~prefix:"-ERR" line -> reader_loop t buf
  | line
    when String.starts_with ~prefix:"MSG" line
         || String.starts_with ~prefix:"HMSG" line ->
    let msg = parse_msg line buf in
    dispatch_msg t msg;
    reader_loop t buf
  | _ -> reader_loop t buf

(** {2 Connection lifecycle} *)

let unsub self ?max_msgs sid =
  send_unsub self ~sid ~max_msgs;
  let@ () = Eio.Mutex.use_rw ~protect:true self.subs_mutex in
  Int_tbl.remove self.subs sid

let sub self ~sw ~subject ?queue f =
  let subject = subject_of_list_for_sub subject in
  let sid = Atomic.fetch_and_add self.next_sid 1 in
  send_sub self ~sid subject queue;
  Eio.Mutex.use_rw ~protect:true self.subs_mutex (fun () ->
      Int_tbl.replace self.subs sid { f });
  Eio.Switch.on_release sw (fun () -> unsub self sid);
  sid

let parse_ip host = Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string host)

let connect_with_addr ~sw ~net ?token ?user ?pass addr =
  let flow = Eio.Net.connect ~sw net addr in
  let buf = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) flow in
  let is_done, resolve_is_done = Eio.Promise.create () in
  let inbox_prefix = make_inbox_prefix () in
  let t =
    {
      flow :> Eio.Flow.sink_ty Eio.Flow.sink;
      write_mutex = Eio.Mutex.create ();
      sw;
      subs = Int_tbl.create 16;
      subs_mutex = Eio.Mutex.create ();
      next_sid = Atomic.make 1;
      is_done;
      shutdown =
        (fun () ->
          if not (Eio.Promise.is_resolved is_done) then (
            Eio.Promise.resolve resolve_is_done ();
            Eio.Flow.shutdown flow `All
          ));
      inbox_prefix;
      inbox_counter = Atomic.make 0;
      inbox_promises = Int_tbl.create 16;
      inbox_promises_mutex = Eio.Mutex.create ();
    }
  in
  let _inbox_sub : sub =
    sub t ~sw ~subject:[ "_INBOX"; inbox_prefix; "*" ] (fun _ -> ())
  in
  let info_line = Eio.Buf_read.line buf in
  if not (String.starts_with ~prefix:"INFO" info_line) then
    failwith (spf "expected INFO, got: %S" info_line);
  send_connect t ?token ?user ?pass ();
  Eio.Fiber.fork_daemon ~sw (fun () ->
      try reader_loop t buf with End_of_file -> `Stop_daemon);
  t

let connect ~sw ~net ?token ?user ?pass ?host ?(port = 4222) () =
  let host =
    match host with
    | Some h -> parse_ip h
    | None -> Eio.Net.Ipaddr.V4.loopback
  in
  let addr = `Tcp (host, port) in
  connect_with_addr ~sw ~net ?token ?user ?pass addr

let close self = self.shutdown ()

let with_connect ~sw ~net ?token ?user ?pass ?host ?port () f =
  let c = connect ~sw ~net ?token ?user ?pass ?host ?port () in
  Fun.protect (fun () -> f c) ~finally:(fun () -> close c)

let pub self ~subject ?reply_to payload =
  let subject = subject_of_list subject in
  send_pub self ~subject ~reply_to ~payload

let hpub self ~subject ?reply_to ?(headers = []) payload =
  let subject = subject_of_list subject in
  send_hpub self ~subject ~reply_to ~headers ~payload

let request self ~sw:_ ~clock ~subject ~timeout payload =
  let counter = Atomic.fetch_and_add self.inbox_counter 1 in
  let inbox = spf "_INBOX.%s.%d" self.inbox_prefix counter in
  let p, r = Eio.Promise.create () in
  Eio.Mutex.use_rw ~protect:true self.inbox_promises_mutex (fun () ->
      Int_tbl.replace self.inbox_promises counter r);
  pub self ~subject ~reply_to:inbox payload;
  match
    Eio.Time.with_timeout clock timeout (fun () -> Ok (Eio.Promise.await p))
  with
  | Ok x -> Ok x
  | Error `Timeout ->
    Eio.Mutex.use_rw ~protect:true self.inbox_promises_mutex (fun () ->
        Int_tbl.remove self.inbox_promises counter);
    Error `Timeout

let wait self = Eio.Promise.await self.is_done

(** {2 Retry helper} *)

let with_retry ~clock ?(delay = 15.) ?(max_retries : int option) ~connect () f =
  let rec loop count =
    match connect () with
    | conn ->
      let result = f conn in
      close conn;
      result
    | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      (match max_retries with
      | Some max when count >= max -> Printexc.raise_with_backtrace exn bt
      | _ ->
        Eio.Time.sleep clock delay;
        loop (count + 1))
  in
  loop 1

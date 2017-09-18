
(* This file is free software. See file "license" for more details. *)

(** {1 Notifications} *)

type t = {
  notify: string -> unit;
  sync: unit -> unit;
}

let mk_ notify sync : t = {notify;sync}

let send (n:t) s = n.notify s
let sendf (n:t) fmt = CCFormat.ksprintf ~f:n.notify fmt
let sync (n:t): unit = n.sync()

let nil : t = mk_ (fun _ ->()) (fun _ -> ())

let stdout : t = mk_ print_endline (fun _ -> flush stdout)

let combine a b : t =
  mk_ (fun x -> a.notify x; b.notify x) (fun () -> a.sync(); b.sync())

let combine_l l : t =
  mk_
    (fun x -> List.iter (fun n -> send n x) l)
    (fun() -> List.iter (fun n->n.sync()) l)

(* IRC notification *)
let mk_irc_ config : t option =
  let module I = Irc_client_unix in
  let module Q = CCBlockingQueue in
  let server = Config.get_or ~default:"irc.freenode.net" config (Config.string "irc-server") in
  let port = Config.get_or ~default:6667 config (Config.int "irc-port") in
  let channel = Config.get_or ~default:"#nunchaku" config (Config.string "irc-chan") in
  let nick = Config.get_or ~default:"logitest-bot" config (Config.string "irc-nick") in
  Misc.Debug.debugf 1 (fun k->k "trying to connect to IRC on `%s:%d`…" server port);
  begin match I.connect_by_name ~server ~port ~nick () with
    | Some connection ->
      Misc.Debug.debugf 1 (fun k->k "connected! now joining `%s`…" channel);
      I.send_join ~connection ~channel;
      Misc.Debug.debug 1 "joined.";
      let chan = Q.create 25 in
      (* to send a notification, push onto queue. If queue is full,
         just ignore. *)
      let n =
        mk_
          (fun s -> ignore (Q.try_push chan s))
          (fun () -> I.send_quit ~connection)
      in
      (* thread to keep connection alive *)
      let th1 =
        CCThread.spawn
          (fun () ->
             let callback _ = function
               | Result.Ok msg ->
                 Misc.Debug.debugf 2
                   (fun k->k "received IRC msg `%s`" (Irc_message.to_string msg));
               | Result.Error e ->
                 Misc.Debug.debugf 2
                   (fun k->k "received IRC error `%s`" e);
             in
             I.listen ~keepalive:{I.mode=`Active;timeout=30} ~connection ~callback ())
      (* thread to send messages without blocking *)
      and th2 =
        CCThread.spawn
          (fun () ->
             while true do
               let message = Q.take chan in
               Misc.Debug.debugf 5(fun k->k "notify on IRC `%s`" message);
               I.send_privmsg ~connection ~target:channel ~message
             done)
      in
      let cleanup() =
        Thread.kill th1;
        Thread.kill th2;
        I.send_quit ~connection
      in
      Gc.finalise (fun _ -> cleanup()) n;
      Some n
    | None ->
      Misc.Debug.debug 1 "could not connect to IRC";
      None
  end

let make ?(irc=false) (config:Config.t) : t =
  (* notification by IRC, if any *)
  let n_irc = if irc then CCOpt.to_list (mk_irc_ config) else [] in
  combine_l (stdout :: n_irc)


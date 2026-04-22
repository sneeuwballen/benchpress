(* This file is free software. See file "license" for more details. *)

(** Networking and socket utilities *)

let pp_inet_addr fmt a = Format.fprintf fmt "%s" (Unix.string_of_inet_addr a)

let pp_unix_addr fmt addr =
  match addr with
  | Unix.ADDR_UNIX s -> Format.fprintf fmt "ADDR_UNIX %s" s
  | Unix.ADDR_INET (addr, id) ->
    Format.fprintf fmt "ADDR_INET (%a, %d)" pp_inet_addr addr id

let ip_addr_conv =
  Cmdliner.Arg.conv
    ( (fun str ->
        try Ok (Unix.inet_addr_of_string str) with Failure s -> Error (`Msg s)),
      fun fmt addr -> Format.fprintf fmt "%s" (Unix.string_of_inet_addr addr) )

let localhost_addr () =
  (Unix.gethostbyname (Unix.gethostname ())).Unix.h_addr_list.(0)

let rec accept_non_intr s =
  try Unix.accept ~cloexec:true s
  with Unix.Unix_error (EINTR, _, _) -> accept_non_intr s

(** [mk_socket sockaddr] makes a socket, binds it to [sockaddr] and returns it
    along with the name of the service if it is a unix socket or the port number
    if it is an internet socket. *)
let mk_socket sockaddr =
  let open Unix in
  let sock = socket ~cloexec:true (domain_of_sockaddr sockaddr) SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock sockaddr;
  sock, (Unix.getnameinfo (Unix.getsockname sock) []).ni_service

(** [start_server n server_fun sock] starts a server on the socket [sock],
    assumes that the socket is correcly bound to a valid address. Allows up to
    [n] connections and runs the function [server_fun] for each connection on a
    separate thread (uses threads, doesn't fork the process).*)
let start_server n server_fun sock =
  let open Unix in
  listen sock 5;
  let threads =
    List.init n (fun _ ->
        Thread.create
          (fun () ->
            let s, _caller = accept_non_intr sock in
            let inchan = in_channel_of_descr s in
            let outchan = out_channel_of_descr s in
            server_fun inchan outchan)
          ())
  in
  List.iter Thread.join threads

(** [establish_server n server_fun sockaddr] same as [Unix.establish_server],
    but it uses threads instead of forking the process after each connection,
    and only accepts [n] connections *)
let establish_server n server_fun sockaddr =
  let sock, _ = mk_socket sockaddr in
  start_server n server_fun sock

(** [mk_shell_cmd ?options ?target exec] makes a command that executes [exec]
    with the options [options] on the target [target]. *)
let mk_shell_cmd ?(options = []) ?target exec =
  let buf = Buffer.create 32 in
  Buffer.add_string buf exec;
  List.iter
    (fun (k, v_opt) ->
      Buffer.add_string buf
        (match v_opt with
        | Some v -> " " ^ k ^ " " ^ v
        | None -> " " ^ k))
    options;
  CCOption.iter (fun s -> Buffer.add_string buf (" " ^ s)) target;
  Buffer.contents buf

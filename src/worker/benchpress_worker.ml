let parse_cmdline =
  let open Cmdliner in
  let aux defs id socket_addr_opt socket_port j timeout memory =
    try
      let socket_addr =
        match socket_addr_opt with
        | Some sa -> sa
        | None ->
          Error.failf
            "Provide an IP address for the socket of the master with the \
             command line argument `-i`."
      in
      if id < 0 || socket_port < 0 then
        Error.failf
          "The values of the worker id `-i` and the port of the socket of the \
           master need to be provided and to be strictly positive integers.";
      Worker.run_worker ?timeout ?memory defs id socket_addr socket_port j;
      true
    with Error.E e -> Error.failf "%a@." Error.pp e
  in
  let defs = Bin_utils.definitions_term
  and id =
    Arg.(
      value & opt int (-1)
      & info [ "i"; "id" ] ~doc:"unique stricly positive integer worker ID")
  and j =
    Arg.(
      value
      & opt int (Misc.guess_cpu_count ())
      & info [ "j" ] ~doc:"level of parallelism")
  and socket_addr_opt =
    Arg.(
      value
      & opt (some Misc.ip_addr_conv) None
      & info [ "a"; "addr" ] ~doc:"IP socket address")
  and socket_port =
    Arg.(value & opt int (-1) & info [ "p"; "port" ] ~doc:"IP socket port")
  and timeout =
    Arg.(
      value
      & opt (some int) None
      & info [ "t"; "timeout" ] ~doc:"timeout (in seconds)")
  and memory =
    Arg.(
      value & opt (some int) None & info [ "m"; "memory" ] ~doc:"memory (in MB)")
  in
  let doc = "" in
  Cmd.v (Cmd.info ~doc "run")
    Term.(
      const aux $ defs $ id $ socket_addr_opt $ socket_port $ j $ timeout
      $ memory)

let () =
  match Cmdliner.Cmd.eval_value parse_cmdline with
  | Error (`Parse | `Term | `Exn) -> exit 2
  | Ok (`Ok true | `Version | `Help) -> ()
  | Ok (`Ok false) -> exit 1

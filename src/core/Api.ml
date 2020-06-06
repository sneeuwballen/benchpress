
include Api_types
include Api_pp
include Api_pb

(** Port to listen on by default *)
let default_port = 8090

(** Helper to serialize a value into protobuf *)
let pb_to_string (f: 'a -> Pbrt.Encoder.t -> unit) (x:'a) : string =
  let e = Pbrt.Encoder.create() in
  f x e;
  Pbrt.Encoder.to_string e

let pb_of_string (f:Pbrt.Decoder.t -> 'a) (s:string) : 'a =
  let d = Pbrt.Decoder.of_string s in
  try f d
  with e ->
    failwith ("protobuf decoding error: " ^ Printexc.to_string e)

let unwrap_field_ what = function
  | Some x -> x
  | None -> failwith ("tried to unwrap protobuf field " ^ what)

module Log = (val Logs.src_log (Logs.Src.create "api"))

module Server : sig
  type t
  val create : port:int -> t

  val add :
    t ->
    string ->
    dec:(Pbrt.Decoder.t -> 'a) ->
    enc:('b -> Pbrt.Encoder.t -> unit) ->
    ('a -> ('b,string) result) ->
    unit

  val serve : t -> unit
end = struct
  type fun_ =
      Fun : {
        dec:(Pbrt.Decoder.t -> 'a);
        enc:('b -> Pbrt.Encoder.t -> unit);
        f:('a -> ('b,string) result);
      } -> fun_

  type t = {
    mutable cl_id: int; (* client id *)
    funs: (string, fun_) Hashtbl.t;
    port: int;
  }

  let create ~port : t =
    { cl_id=0; port; funs=Hashtbl.create 16 }

  let add self name ~dec ~enc f : unit =
    Hashtbl.replace self.funs name (Fun {dec;enc;f})

  let write_res_ok_ oc s : unit =
    Printf.fprintf oc "%d OK\r\n%s%!" (String.length s) s
  let write_res_err_ oc s : unit =
    Printf.fprintf oc "%d ERR\r\n%s%!" (String.length s) s

  let process_client (self:t) ~cl_id (ic,oc) : unit =
    try
      while true do
        let line = input_line ic |> String.trim in
        (* header: [<body-len> <name>\n]
           where [<body-len>] is the number of bytes for the body *)
        let len, meth =
          try
            let i = String.index line ' ' in
            int_of_string (String.sub line 0 i),
            String.trim (String.sub line i (String.length line-i))
          with _ ->
            Log.err (fun k->k"invalid header line: %S" line);
            raise Exit
        in
        Log.debug (fun k->k
                      "read request header: %S body-len=%d meth=%s cl_id=%d"
                      line len meth cl_id);
        (* read body and decode it *)
        let body = really_input_string ic len in
        begin match Hashtbl.find self.funs meth with
          | Fun {dec;enc;f} ->
            let x = pb_of_string dec body in
            begin match f x with
              | Ok y ->
                let res = pb_to_string enc y in
                Log.debug (fun k->k"successful function call cl_id=%d" cl_id);
                write_res_ok_ oc res
              | Error e ->
                Log.debug (fun k->k"failed function call cl_id=%d err=%S" cl_id e);
                write_res_err_ oc e
            end
          | exception Not_found ->
            write_res_err_ oc "method not found"
        end;
      done
    with
    | End_of_file ->
      Log.debug (fun k->k "client closed connection cl_id=%d" cl_id);
      close_in_noerr ic;
      close_out_noerr oc;
    | e ->
      let e = Printexc.to_string e in
      Log.warn (fun k->k "kind=server cl_id=%d err=%s" cl_id e);
      (try write_res_err_ oc e with _ -> ());
      close_in_noerr ic;
      close_out_noerr oc;
      ()

  let serve (self:t) : unit =
    Log.info (fun k->k"serve API on port %d" self.port);
    try
      let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.setsockopt sock Unix.SO_REUSEADDR true;
      Unix.setsockopt_optint sock Unix.SO_LINGER None;
      Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, self.port));
      Unix.listen sock 16;
      while true do
        let fd, _addr = Unix.accept sock in
        let cl_id = self.cl_id in
        self.cl_id <- 1 + self.cl_id;
        let ic = Unix.in_channel_of_descr fd in
        let oc = Unix.out_channel_of_descr fd in
        Log.debug (fun k->k "new connection cl_id=%d" cl_id);
        let _th = Thread.create (process_client self ~cl_id) (ic,oc) in
        ()
      done
    with e ->
      Log.err (fun k->k "api server: %s" (Printexc.to_string e));
      ()
end

module Client : sig
  type t

  val create : ?host:string -> ?port:int -> unit -> t

  val call :
    t ->
    string ->
    enc:('a -> Pbrt.Encoder.t -> unit) ->
    dec:(Pbrt.Decoder.t -> 'b) ->
    'a ->
    ('b, string) result

  val close : t -> unit
end = struct
  type conn = {
    ic: in_channel;
    oc: out_channel;
  }
  type t = {
    cl_id: int;
    host: string;
    port: int;
    mutable bad_tries: int;
    mutable conn: conn option;
  }

  let cl_id_ = ref 0

  let create ?(host="127.0.0.1") ?(port=default_port) () : t =
    let cl_id = !cl_id_ in
    incr cl_id_;
    Log.debug (fun k->k"create client cl_id=%d host=%S port=%d" cl_id host port);
    { host; port; conn=None; bad_tries=0; cl_id; }

  let max_bad_tries = 5

  let rec mk_conn_ (self:t) : conn =
    if self.bad_tries > max_bad_tries then (
      failwith "maximum number of bad tries exceeded, give up"
    ) else (
      match
        let addr = Unix.ADDR_INET(Unix.inet_addr_of_string self.host, self.port) in
        Unix.open_connection addr
      with
      | ic, oc ->
        self.bad_tries <- 0;
        {ic;oc}
      | exception e ->
        Logs.debug
          (fun k->k "error when trying to connect to API: %s" (Printexc.to_string e));
        (* try again *)
        self.bad_tries <- 1 + self.bad_tries;
        mk_conn_ self
    )

  let get_or_create_conn_ self =
    match self.conn with
    | Some c -> c
    | None ->
      let c = mk_conn_ self in
      self.conn <- Some c;
      c

  let write_call_ oc name body : unit =
    Printf.fprintf oc "%d %s\r\n%s%!" (String.length body) name body

  let call (self:t) name ~enc ~dec (x:'a) : _ result =
    try
      (* I do not like sigpipe, no I don't *)
      ignore (Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigpipe] : _ list);
      (* get connection *)
      let conn = get_or_create_conn_ self in
      Log.debug (fun k->k "client.call meth=%S cl_id=%d" name self.cl_id);
      let body = pb_to_string enc x in
      write_call_ conn.oc name body;
      Log.debug (fun k->k "client.read_res cl_id=%d" self.cl_id);
      let line = input_line conn.ic in
      let len, success =
        try
          let i = String.index line ' ' in
          let succ =
            String.lowercase_ascii @@ String.trim @@
            String.sub line i (String.length line-i)
          in
          int_of_string (String.sub line 0 i),
          (match succ with
           | "ok" -> true | "err" -> false
           | s -> failwith ("invalid response code: " ^ s))
        with e ->
          failwith @@ "invalid status line: " ^ Printexc.to_string e
      in
      let body = really_input_string conn.ic len in
      if success then (
        try
          let x = pb_of_string dec body in
          Ok x
        with e ->
          Error ("cannot decode response body: " ^ Printexc.to_string e)
      ) else (
        Error body
      )
    with e ->
      Error (Printexc.to_string e)

  let close self : unit =
    Log.debug (fun k->k"client.close cl_id=%d" self.cl_id);
    let conn = self.conn in
    self.conn <- None;
    self.bad_tries <- max_bad_tries + 1; (* will not work anymore *)
    CCOpt.iter (fun c -> close_in_noerr c.ic; close_out_noerr c.oc) conn;
    ()
end

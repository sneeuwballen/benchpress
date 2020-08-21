
module E = CCResult
module R = Redis_sync.Client
include Api_types

let spf = Printf.sprintf

(** Default port for redis. *)
let default_port = 6379

type 'a or_error = ('a, string) result

module Log = (val Logs.src_log (Logs.Src.create "api"))

module Client : sig
  type t

  val create : ?host:string -> ?port:int -> unit -> t

  val with_lock : t -> id:string -> (unit -> 'a) -> 'a or_error
  (** acquire lock, call function in the lock scope.
      This is intended for system-wide, long held locks. *)

  val set_active : t -> task_descr -> unit or_error

  val set_waiting : t -> task_descr -> unit or_error

  val get_task_list : t -> task_list or_error

  (* TODO
  val call :
    t ->
    string ->
    enc:('a -> Pbrt.Encoder.t -> unit) ->
    dec:(Pbrt.Decoder.t -> 'b) ->
    'a ->
    ('b, string) result
     *)

  val close : t -> unit
end = struct
  type conn = R.connection
  type t = {
    cl_id: int;
    host: string;
    port: int;
    mutable bad_tries: int;
    mutable conn: conn option;
    mutable gaveup_at: float; (* timestamp for last time we gave up *)
  }

  let cl_id_ = ref 0

  let create ?(host="127.0.0.1") ?(port=default_port) () : t =
    let cl_id = !cl_id_ in
    incr cl_id_;
    Log.debug (fun k->k"create client cl_id=%d host=%S port=%d" cl_id host port);
    { host; port; conn=None; bad_tries=0; cl_id; gaveup_at=0.; }

  (* cool off before we try again *)
  let cool_off = 30.

  (* maximum number of failed connections before giving up *)
  let max_bad_tries = 5

  let rec mk_conn_ (self:t) : conn =
    if self.bad_tries > max_bad_tries then (
      self.gaveup_at <- Unix.gettimeofday();
      failwith "maximum number of bad tries exceeded, give up"
    ) else (
      let spec = {R.host=self.host; port=self.port} in
      match R.connect spec with
      | conn ->
        self.bad_tries <- 0;
        conn
      | exception e ->
        Logs.debug
          (fun k->k "error when trying to connect to API: %s" (Printexc.to_string e));
        (* try again, after 1s *)
        self.bad_tries <- 1 + self.bad_tries;
        Thread.delay 1.;
        mk_conn_ self
    )

  let get_or_create_conn_ self =
    match self.conn with
    | Some c -> c
    | None ->
      if Unix.gettimeofday () -. self.gaveup_at > cool_off then (
        (* we can try again *)
        self.bad_tries <- 0;
      );
      let c = mk_conn_ self in
      self.conn <- Some c;
      c

  (* begin API *)

  let with_lock (self:t) ~id:job_id f =
    match mk_conn_ self with
    | exception _ ->
      Error "lock: could not acquire connection to redis"
    | c ->
      let tid = Thread.id (Thread.self()) in
      let key = spf "lock:%d:%s" tid job_id in
      let has_finished = CCLock.create false in
      (* poll to acquire the lock *)
      while not @@ R.set c ~nx:true ~ex:10 key "locked" do
        Thread.delay 1.;
      done;
      Log.debug (fun k->k "lock acquired for job key=%S" key);
      (* refresh the lock *)
      let _ = Thread.create (fun () ->
          while not @@ CCLock.get has_finished do
            ignore (R.set c ~xx:true ~ex:10 key "locked" : bool);
            Thread.delay 1.
          done)
          ()
      in
      E.guard_str_trace @@ fun () ->
      CCFun.finally
        ~h:(fun () ->
            Log.debug (fun k->k "releasing lock for job key=%S" key);
            CCLock.set has_finished true;
            R.del c [key])
        ~f

  let set_active (self:t) (d:task_descr) =
    E.guard_str_trace @@ fun () ->
    let conn = mk_conn_ self in
    Ok ()

  let set_waiting (self:t) (d:task_descr) =
    E.guard_str_trace @@ fun () ->
    let conn = mk_conn_ self in
    Ok ()

  let get_task_list (self:t) =
    E.guard_str_trace @@ fun () ->
    let conn = mk_conn_ self in
    R.
    Ok ()


  (*
  let write_call_ oc name body : unit =
    Printf.fprintf oc "%d %s\r\n%s%!" (String.length body) name body

  let call (self:t) name ~enc ~dec (x:'a) : _ result =
    Log.debug (fun k->k "make API call %s" name);
    try
      (* I do not like sigpipe, no I don't *)
      ignore (Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigpipe] : _ list);
      (* get connection *)
      let conn =
        try get_or_create_conn_ self
        with e ->
          self.conn <- None;
          raise e
      in
      Log.debug (fun k->k "client.call meth=%S cl_id=%d" name self.cl_id);
      let body = pb_to_string enc x in
      begin
        try write_call_ conn.oc name body
        with e ->
          self.conn <- None; (* will have to retry *)
          raise e
      end;
      Log.debug (fun k->k "client.read_res cl_id=%d" self.cl_id);
      let line =
        try input_line conn.ic
        with e -> self.conn <- None; raise e
      in
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
      let body =
        try really_input_string conn.ic len
        with e -> self.conn <- None; raise e
      in
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
     *)

  let close self : unit =
    Log.debug (fun k->k"client.close cl_id=%d" self.cl_id);
    let conn = self.conn in
    self.conn <- None;
    self.bad_tries <- max_bad_tries + 1; (* will not work anymore *)
    CCOpt.iter R.disconnect conn;
    ()
end

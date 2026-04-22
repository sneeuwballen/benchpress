open Common

type t = { db: Db.t; mu: Mutex.t }

let schema =
  {|
  CREATE TABLE IF NOT EXISTS user (
    id         TEXT NOT NULL PRIMARY KEY,
    email      TEXT NOT NULL UNIQUE,
    created_at TEXT NOT NULL
  );

  CREATE TABLE IF NOT EXISTS api_key (
    key        TEXT NOT NULL PRIMARY KEY,
    user_id    TEXT NOT NULL REFERENCES user(id),
    created_at TEXT NOT NULL
  );

  CREATE INDEX IF NOT EXISTS api_key_user ON api_key(user_id);

  CREATE TABLE IF NOT EXISTS job (
    id          TEXT NOT NULL PRIMARY KEY,
    user_id     TEXT NOT NULL,
    cancelled   INTEGER NOT NULL DEFAULT 0,
    completed   INTEGER NOT NULL DEFAULT 0,
    result_file TEXT,
    created_at  TEXT NOT NULL
  );
|}

let create (path : string) : t =
  let db = Sqlite3.db_open path in
  Db.exec0 db schema |> Misc.unwrap_db (fun () -> "auth.create");
  { db; mu = Mutex.create () }

let default_path () : string =
  let dir = Xdg.data_dir () in
  let d = Filename.concat dir "benchpress" in
  Misc.mkdir_rec d;
  Filename.concat d "auth.db"

let now_str () : string = Ptime.to_rfc3339 (Ptime_clock.now ())

let gen_key () : string =
  let s = Mirage_crypto_rng.generate 32 in
  String.concat ""
    (List.init (String.length s) (fun i ->
         Printf.sprintf "%02x" (Char.code s.[i])))

let mk_uuid () : string =
  Uuidm.to_string (Uuidm.v4_gen (Random.State.make_self_init ()) ())

let create_user (self : t) ~(email : string) : (string, string) result =
  Mutex.lock self.mu;
  Fun.protect ~finally:(fun () -> Mutex.unlock self.mu) @@ fun () ->
  let id = mk_uuid () in
  let now = now_str () in
  try
    Db.exec_no_cursor self.db
      {|INSERT INTO user (id, email, created_at) VALUES (?, ?, ?)|}
      ~ty:Db.Ty.[ text; text; text ]
      id email now
    |> Misc.unwrap_db (fun () -> "auth.create_user");
    Ok id
  with
  | Sqlite3.Error msg -> Error msg
  | Error.E e -> Error (Error.show e)

let create_api_key (self : t) ~(user_id : string) : (string, string) result =
  Mutex.lock self.mu;
  Fun.protect ~finally:(fun () -> Mutex.unlock self.mu) @@ fun () ->
  (* check user exists *)
  let exists =
    Db.exec self.db {|SELECT id FROM user WHERE id=?|}
      ~ty:Db.Ty.(p1 text, p1 text, fun s -> s)
      user_id
      ~f:(fun cursor -> Db.Cursor.next cursor <> None)
  in
  match exists with
  | Error rc -> Error (Db.Rc.to_string rc)
  | Ok false -> Error (Printf.sprintf "user not found: %s" user_id)
  | Ok true ->
    let key = gen_key () in
    let now = now_str () in
    (try
       Db.exec_no_cursor self.db
         {|INSERT INTO api_key (key, user_id, created_at) VALUES (?, ?, ?)|}
         ~ty:Db.Ty.[ text; text; text ]
         key user_id now
       |> Misc.unwrap_db (fun () -> "auth.create_api_key");
       Ok key
     with
    | Sqlite3.Error msg -> Error msg
    | Error.E e -> Error (Error.show e))

let revoke_api_key (self : t) ~(key : string) : unit =
  Mutex.lock self.mu;
  Fun.protect ~finally:(fun () -> Mutex.unlock self.mu) @@ fun () ->
  Db.exec_no_cursor self.db {|DELETE FROM api_key WHERE key=?|}
    ~ty:Db.Ty.[ text ]
    key
  |> Misc.unwrap_db (fun () -> "auth.revoke_api_key")

let list_api_keys (self : t) ~(user_id : string) : (string * string) list =
  Mutex.lock self.mu;
  Fun.protect ~finally:(fun () -> Mutex.unlock self.mu) @@ fun () ->
  Db.exec self.db {|SELECT key, created_at FROM api_key WHERE user_id=?|}
    ~ty:Db.Ty.(p1 text, p2 text text, mkp2)
    user_id ~f:Db.Cursor.to_list_rev
  |> Misc.unwrap_db (fun () -> "auth.list_api_keys")

let authenticate (self : t) ~(key : string) : string option =
  Mutex.lock self.mu;
  Fun.protect ~finally:(fun () -> Mutex.unlock self.mu) @@ fun () ->
  Db.exec self.db {|SELECT user_id FROM api_key WHERE key=?|}
    ~ty:Db.Ty.(p1 text, p1 text, fun s -> s)
    key ~f:Db.Cursor.next
  |> Misc.unwrap_db (fun () -> "auth.authenticate")

let list_users (self : t) : (string * string * string) list =
  Mutex.lock self.mu;
  Fun.protect ~finally:(fun () -> Mutex.unlock self.mu) @@ fun () ->
  Db.exec_no_params self.db {|SELECT id, email, created_at FROM user|}
    ~ty:Db.Ty.(p3 text text text, mkp3)
    ~f:Db.Cursor.to_list_rev
  |> Misc.unwrap_db (fun () -> "auth.list_users")

let register_job (self : t) ~(job_id : string) ~(user_id : string) : unit =
  Mutex.lock self.mu;
  Fun.protect ~finally:(fun () -> Mutex.unlock self.mu) @@ fun () ->
  let now = now_str () in
  Db.exec_no_cursor self.db
    {|INSERT INTO job (id, user_id, created_at) VALUES (?, ?, ?)|}
    ~ty:Db.Ty.[ text; text; text ]
    job_id user_id now
  |> Misc.unwrap_db (fun () -> "auth.register_job")

let get_job_user (self : t) ~(job_id : string) : string option =
  Mutex.lock self.mu;
  Fun.protect ~finally:(fun () -> Mutex.unlock self.mu) @@ fun () ->
  Db.exec self.db {|SELECT user_id FROM job WHERE id=?|}
    ~ty:Db.Ty.(p1 text, p1 text, fun s -> s)
    job_id ~f:Db.Cursor.next
  |> Misc.unwrap_db (fun () -> "auth.get_job_user")

let set_job_cancelled (self : t) ~(job_id : string) : unit =
  Mutex.lock self.mu;
  Fun.protect ~finally:(fun () -> Mutex.unlock self.mu) @@ fun () ->
  Db.exec_no_cursor self.db {|UPDATE job SET cancelled=1 WHERE id=?|}
    ~ty:Db.Ty.[ text ]
    job_id
  |> Misc.unwrap_db (fun () -> "auth.set_job_cancelled")

let set_job_completed (self : t) ~(job_id : string) ~(result_file : string) :
    unit =
  Mutex.lock self.mu;
  Fun.protect ~finally:(fun () -> Mutex.unlock self.mu) @@ fun () ->
  (* Only update if not already completed, so repeated calls are no-ops. *)
  Db.exec_no_cursor self.db
    {|UPDATE job SET completed=1, result_file=? WHERE id=? AND completed=0|}
    ~ty:Db.Ty.[ text; text ]
    result_file job_id
  |> Misc.unwrap_db (fun () -> "auth.set_job_completed")

let get_job_cancelled (self : t) ~(job_id : string) : bool =
  Mutex.lock self.mu;
  Fun.protect ~finally:(fun () -> Mutex.unlock self.mu) @@ fun () ->
  let r =
    Db.exec self.db {|SELECT cancelled FROM job WHERE id=?|}
      ~ty:Db.Ty.(p1 text, p1 int, fun x -> x)
      job_id ~f:Db.Cursor.next
    |> Misc.unwrap_db (fun () -> "auth.get_job_cancelled")
  in
  match r with
  | Some 1 -> true
  | _ -> false

let get_job_completed (self : t) ~(job_id : string) : string option =
  Mutex.lock self.mu;
  Fun.protect ~finally:(fun () -> Mutex.unlock self.mu) @@ fun () ->
  let r =
    Db.exec self.db {|SELECT completed, result_file FROM job WHERE id=?|}
      ~ty:Db.Ty.(p1 text, p2 int (nullable text), mkp2)
      job_id ~f:Db.Cursor.next
    |> Misc.unwrap_db (fun () -> "auth.get_job_completed")
  in
  match r with
  | Some (1, f) -> Some (Option.value f ~default:"")
  | _ -> None

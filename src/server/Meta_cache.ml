open Benchpress.Common

type t = { database: Db.t; memory: (string, Test_metadata.t) Hashtbl.t }

let createdb (db : Db.t) =
  Db.exec0 db
    {|
    CREATE TABLE IF NOT EXISTS test_database (
      id INTEGER PRIMARY KEY,
      path BLOB NOT NULL UNIQUE
    ) STRICT;

    CREATE TABLE IF NOT EXISTS test_meta (
      database_id INTEGER NOT NULL REFERENCES test_database (id),
      key TEXT NOT NULL,
      value BLOB,
      CONSTRAINT meta_key_unique UNIQUE (database_id, key)
    ) STRICT;

    CREATE TABLE IF NOT EXISTS test_dirs (
      id INTEGER PRIMARY KEY,
      database_id INTEGER NOT NULL REFERENCES test_database (id),
      dir BLOB NOT NULL
    ) STRICT;

    CREATE TABLE IF NOT EXISTS test_provers (
      id INTEGER PRIMARY KEY,
      database_id INTEGER NOT NULL REFERENCES test_database (id),
      prover TEXT NOT NULL
    ) STRICT;
    |}

let get (db : Db.t) self field =
  Option.join
    (Db.exec db ~f:Db.Cursor.next
       ~ty:Db.Ty.(p2 int text, p1 (nullable blob), id)
       {|
        SELECT value FROM test_meta
        WHERE database_id = ? AND key = ?;
       |}
       self field
    |> Misc.unwrap_db (fun () -> spf "Could not read field: '%s'" field))

let load_meta (db : Db.t) self =
  let uuid = get db self "uuid" in
  let uuid = Option.bind uuid Uuidm.of_string in
  let uuid = Option.get uuid in
  let timestamp = get db self "timestamp" in
  let timestamp = Option.map float_of_string timestamp in
  let total_wall_time = get db self "total_wall_time" in
  let total_wall_time = Option.map float_of_string total_wall_time in
  let n_results = get db self "n_results" in
  let n_results = Option.map int_of_string n_results in
  let n_results = Option.value ~default:0 n_results in
  let n_bad = get db self "n_bad" in
  let n_bad = Option.map int_of_string n_bad in
  let n_bad = Option.value ~default:0 n_bad in
  let dirs =
    Db.exec
      ~ty:Db.Ty.(p1 int, p1 blob, id)
      ~f:Db.Cursor.to_list_rev db
      {| SELECT dir FROM test_dirs WHERE database_id = ?; |} self
    |> Misc.unwrap_db (fun () -> spf "Could not load dirs")
  in
  let provers =
    Db.exec
      ~ty:Db.Ty.(p1 int, p1 text, id)
      ~f:Db.Cursor.to_list_rev db
      {| SELECT prover FROM test_provers WHERE database_id = ?; |} self
    |> Misc.unwrap_db (fun () -> spf "Could not load provers")
  in
  Test_metadata.
    { uuid; timestamp; total_wall_time; n_results; n_bad; dirs; provers }

let set (db : Db.t) self field value =
  Db.exec_no_cursor db
    {|
    INSERT OR REPLACE INTO test_meta (
      database_id, key, value
    ) VALUES (?, ?, ?);
    |}
    ~ty:Db.Ty.(p3 int text (nullable blob))
    self field value
  |> Misc.unwrap_db (fun () -> spf "Could not write field: '%s'" field)

let cache_meta (db : Db.t) self (meta : Test_metadata.t) =
  set db self "uuid" (Some (Uuidm.to_string meta.uuid));
  set db self "timestamp" (CCOption.map string_of_float meta.timestamp);
  set db self "total_wall_time"
    (CCOption.map string_of_float meta.total_wall_time);
  set db self "n_results" (Some (string_of_int meta.n_results));
  set db self "n_bad" (Some (string_of_int meta.n_bad));
  Db.exec_no_cursor db
    ~ty:Db.Ty.(p1 int)
    {| DELETE FROM test_dirs WHERE database_id = ?; |} self
  |> Misc.unwrap_db (fun () -> spf "Could not delete dirs");
  List.iter
    (fun dir ->
      Db.exec_no_cursor db
        ~ty:Db.Ty.(p2 int blob)
        {|INSERT INTO test_dirs (database_id, dir) VALUES (?, ?);|} self dir
      |> Misc.unwrap_db (fun () -> spf "Could not add dir"))
    meta.dirs;
  Db.exec_no_cursor db
    ~ty:Db.Ty.(p1 int)
    {| DELETE FROM test_provers WHERE database_id = ?; |} self
  |> Misc.unwrap_db (fun () -> spf "Could not delete provers");
  List.iter
    (fun prover ->
      Db.exec_no_cursor db
        ~ty:Db.Ty.(p2 int text)
        {|INSERT INTO test_provers (database_id, prover) VALUES (?, ?);|} self
        prover
      |> Misc.unwrap_db (fun () -> spf "Could not add prover"))
    meta.provers

let importdb t (p : string) =
  Db.with_db ~timeout:500 ~cache:`PRIVATE ~mode:`READONLY p (fun meta_db ->
      let meta = Test_metadata.of_db meta_db in
      (* Only cache if complete *)
      if Test_metadata.is_complete meta then (
        Hashtbl.replace t.memory p meta;
        let self =
          Db.exec t.database ~f:Db.Cursor.next
            ~ty:Db.Ty.(p1 text, p1 int, id)
            {| INSERT INTO test_database (path) VALUES (?) RETURNING id; |} p
          |> Misc.unwrap_db (fun () -> spf "Could not create metadata cache")
          |> Error.unwrap_opt' (fun () ->
                 spf "Expected an id for metadata cache")
        in
        cache_meta t.database self meta
      );
      meta)

let find t (p : string) =
  try Hashtbl.find t.memory p
  with Not_found ->
    (match
       Db.exec t.database ~f:Db.Cursor.next
         ~ty:Db.Ty.(p1 text, p1 int, id)
         {| SELECT id FROM test_database WHERE path = ?; |} p
       |> Misc.unwrap_db (fun () -> spf "Could not load metadata cache")
     with
    | None -> importdb t p
    | Some self -> load_meta t.database self)

let find_if_loaded t p = Hashtbl.find_opt t.memory p

let create ~path =
  let db = Sqlite3.db_open ~mutex:`FULL path in
  createdb db
  |> Misc.unwrap_db (fun () ->
         spf "Could not create cache database at: %s" path);
  { database = db; memory = Hashtbl.create ~random:true 16 }

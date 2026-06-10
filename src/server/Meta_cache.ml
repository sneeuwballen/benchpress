open Benchpress.Common

type t = {
  database: Db.t;
  memory: (string, Test_metadata.t Eio.Promise.or_exn) Hashtbl.t;
      (** Cache mapping a benchmark file path to a promise of its metadata. *)
  mutable file_hash_cache: (string, string option) Hashtbl.t;
  mtime_cache: (string, float) Hashtbl.t;
      (** Cache mapping path to file mtime at time of import. *)
  mu: Eio.Mutex.t;
  sw: Eio.Switch.t;
}

let createdb (db : Db.t) =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "meta-cache.createdb" in
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
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "meta-cache.get" in
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
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "meta-cache.load-meta" in
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
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "meta-cache.set" in
  Db.exec_no_cursor db
    {|
    INSERT OR REPLACE INTO test_meta (
      database_id, key, value
    ) VALUES (?, ?, ?);
    |}
    ~ty:Db.Ty.(p3 int text (nullable blob))
    self field value
  |> Misc.unwrap_db (fun () -> spf "Could not write field: '%s'" field)

let compute_file_hash (source_db : Db.t) : string =
  let files =
    Db.exec_no_params source_db
      ~ty:Db.Ty.(p1 any_str, id)
      ~f:Db.Cursor.to_list_rev
      {| SELECT DISTINCT file FROM prover_res ORDER BY file; |}
    |> Misc.unwrap_db (fun () -> "listing distinct files for hash")
  in
  let files =
    List.rev_map String.trim files |> CCList.sort_uniq ~cmp:String.compare
  in
  let all_files = String.concat "\x00" files in
  Digestif.SHA256.(digest_string all_files |> to_hex)

let cache_file_hash (db : Db.t) self source_db =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "meta-cache.cache-file-hash" in
  let hash = compute_file_hash source_db in
  set db self "hash-file-set" (Some hash)

let cache_meta (db : Db.t) self (meta : Test_metadata.t) =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "meta-cache.cache-meta" in
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
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "meta-cache.importdb" in
  Db.with_db ~timeout:500 ~cache:`PRIVATE ~mode:`READONLY p (fun meta_db ->
      let meta = Test_metadata.of_db meta_db in
      (* Only cache if complete *)
      if Test_metadata.is_complete meta then (
        let self =
          Db.exec t.database ~f:Db.Cursor.next
            ~ty:Db.Ty.(p1 any_str, p1 int, id)
            {| INSERT INTO test_database (path) VALUES (?) RETURNING id; |} p
          |> Misc.unwrap_db (fun () ->
                 spf "Could not create metadata cache for %s" p)
          |> Error.unwrap_opt' (fun () ->
                 spf "Expected an id for metadata cache")
        in
        cache_meta t.database self meta;
        cache_file_hash t.database self meta_db
      );
      meta)

let file_mtime path =
  try Some (Unix.LargeFile.stat path).Unix.LargeFile.st_mtime with _ -> None

let find t (path : string) : Test_metadata.t =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "meta-cache.find" in
  let promise =
    Eio.Mutex.use_rw ~protect:true t.mu @@ fun () ->
    let current_mtime = file_mtime path in
    match Hashtbl.find_opt t.mtime_cache path with
    | Some mtime when Some mtime = current_mtime ->
      (match Hashtbl.find_opt t.memory path with
      | Some promise -> promise
      | None -> assert false)
    | _ ->
      (* file changed or deleted; invalidate all caches for this path *)
      Hashtbl.remove t.memory path;
      Hashtbl.remove t.file_hash_cache path;
      Hashtbl.remove t.mtime_cache path;
      let p =
        Eio.Fiber.fork_promise ~sw:t.sw (fun () ->
            match
              Db.exec t.database ~f:Db.Cursor.next
                ~ty:Db.Ty.(p1 any_str, p1 int, id)
                {| SELECT id FROM test_database WHERE path = ?; |} path
              |> Misc.unwrap_db (fun () ->
                     spf "Could not load metadata cache for %s" path)
            with
            | Some self -> load_meta t.database self
            | None -> importdb t path)
      in
      Hashtbl.replace t.memory path p;
      (match current_mtime with
      | Some mtime -> Hashtbl.replace t.mtime_cache path mtime
      | None -> ());
      p
  in
  Eio.Promise.await_exn promise

let find_if_loaded t path =
  let current_mtime = file_mtime path in
  match
    Eio.Mutex.use_ro t.mu (fun () ->
        match Hashtbl.find_opt t.mtime_cache path with
        | Some mtime when Some mtime = current_mtime ->
          Hashtbl.find_opt t.memory path
        | _ -> None)
  with
  | Some p when Eio.Promise.is_resolved p ->
    (try Some (Eio.Promise.await_exn p) with _ -> None)
  | _ -> None

let find_similar_runs (t : t) (file_path : string) : string list =
  let@ _sp =
    Trace.with_span ~__FILE__ ~__LINE__ "meta-cache.find-similar-runs"
  in
  let hash =
    match Hashtbl.find_opt t.file_hash_cache file_path with
    | Some cached -> cached
    | None ->
      let h =
        let self =
          match
            Db.exec t.database ~f:Db.Cursor.next
              ~ty:Db.Ty.(p1 any_str, p1 int, id)
              {| SELECT id FROM test_database WHERE path = ?; |} file_path
            |> Misc.unwrap_db (fun () ->
                   spf "Could not load db id for %s" file_path)
          with
          | Some self -> Some self
          | None ->
            let _meta = find t file_path in
            (match
               Db.exec t.database ~f:Db.Cursor.next
                 ~ty:Db.Ty.(p1 any_str, p1 int, id)
                 {| SELECT id FROM test_database WHERE path = ?; |} file_path
               |> Misc.unwrap_db (fun () ->
                      spf "Could not load db id after import")
             with
            | Some self -> Some self
            | None ->
              (* partial run: not cached in test_database, skip *)
              None)
        in
        match self with
        | Some self ->
          (match get t.database self "hash-file-set" with
          | Some h -> Some h
          | None ->
            (* hash was deleted or not yet cached; recompute and store *)
            Db.with_db ~timeout:500 ~cache:`PRIVATE ~mode:`READONLY file_path
              (fun source_db -> cache_file_hash t.database self source_db);
            get t.database self "hash-file-set")
        | None -> None
      in
      Hashtbl.replace t.file_hash_cache file_path h;
      h
  in
  match hash with
  | None -> []
  | Some h ->
    Db.exec t.database
      ~ty:Db.Ty.(p1 any_str, p1 any_str, id)
      ~f:Db.Cursor.to_list_rev
      {|
        SELECT td.path FROM test_database td
        JOIN test_meta tm ON td.id = tm.database_id
        WHERE tm.key = 'hash-file-set' AND tm.value = ?
        ORDER BY td.path;
      |}
      h
    |> Misc.unwrap_db (fun () -> "finding similar runs")

let remove t path =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "meta-cache.remove" in
  Eio.Mutex.use_rw ~protect:true t.mu @@ fun () ->
  Hashtbl.remove t.memory path;
  Hashtbl.remove t.file_hash_cache path;
  Hashtbl.remove t.mtime_cache path;
  match
    Db.exec t.database ~f:Db.Cursor.next
      ~ty:Db.Ty.(p1 any_str, p1 int, id)
      {| SELECT id FROM test_database WHERE path = ?; |} path
    |> Misc.unwrap_db (fun () -> spf "Could not load db id for %s" path)
  with
  | Some self ->
    Db.exec_no_cursor t.database
      ~ty:Db.Ty.(p1 int)
      {| DELETE FROM test_provers WHERE database_id = ?; |} self
    |> Misc.unwrap_db (fun () -> spf "Could not delete provers");
    Db.exec_no_cursor t.database
      ~ty:Db.Ty.(p1 int)
      {| DELETE FROM test_dirs WHERE database_id = ?; |} self
    |> Misc.unwrap_db (fun () -> spf "Could not delete dirs");
    Db.exec_no_cursor t.database
      ~ty:Db.Ty.(p1 int)
      {| DELETE FROM test_meta WHERE database_id = ?; |} self
    |> Misc.unwrap_db (fun () -> spf "Could not delete meta");
    Db.exec_no_cursor t.database
      ~ty:Db.Ty.(p1 int)
      {| DELETE FROM test_database WHERE id = ?; |} self
    |> Misc.unwrap_db (fun () -> spf "Could not delete from database")
  | None -> ()

let create ~sw ~path =
  let db = Sqlite3.db_open ~mutex:`FULL path in
  createdb db
  |> Misc.unwrap_db (fun () ->
         spf "Could not create cache database at: %s" path);
  {
    database = db;
    memory = Hashtbl.create ~random:true 16;
    file_hash_cache = Hashtbl.create ~random:true 8;
    mtime_cache = Hashtbl.create ~random:true 8;
    mu = Eio.Mutex.create ();
    sw;
  }

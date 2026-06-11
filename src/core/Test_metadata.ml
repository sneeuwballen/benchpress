open Common
open Test
module PB = PrintBox

type t = {
  uuid: Uuidm.t; (* unique ID *)
  timestamp: float option; (* timestamp *)
  total_wall_time: float option;
  n_results: int;
  n_bad: int;
  dirs: string list;
  provers: Prover.name list;
  prover_versions: (Prover.name * Prover.version) list;
}

let is_complete self = CCOpt.is_some self.total_wall_time

let pp out (self : t) : unit =
  let pp_prover_version out (name, version) =
    Fmt.fprintf out "%s (%s)" name version
  in
  Fmt.fprintf out
    "run-metadata:\n\
    \  n-results: %d%s\n\
    \  provers: [%a]\n\
    \  timestamp: %s\n\
    \  total-time: %s\n\
    \  uuid: %a"
    self.n_results
    (if self.n_bad > 0 then
       Printf.sprintf "\n  bad: %d" self.n_bad
     else
       "")
    (Fmt.list ~sep:(Fmt.return "; ") pp_prover_version)
    self.prover_versions
    (CCOpt.map_or ~default:"<no time>" Misc.human_datetime self.timestamp)
    (CCOpt.map_or ~default:"<no wall time>" Misc.human_duration
       self.total_wall_time)
    Uuidm.pp self.uuid

let to_string self = Fmt.asprintf "%a" pp self

let to_printbox ?link:(mk_link = default_linker) self : PB.t =
  let open PB in
  let pp_prover_with_version (name, version) = spf "%s (%s)" name version in
  pb_v_record
  @@ List.flatten
       [
         [
           ( "provers",
             vlist_map mk_link
               (List.map pp_prover_with_version self.prover_versions) );
           "n_results", int self.n_results;
         ];
         (if self.n_bad > 0 then
            [ "bad", int self.n_bad ]
          else
            []);
         [
           "uuid", text @@ Uuidm.to_string self.uuid;
           "dirs", hlist_map text self.dirs;
           ( "timestamp",
             match self.timestamp with
             | None -> text "none"
             | Some f -> text @@ Misc.human_datetime f );
           ( "total_wall_time",
             match self.total_wall_time with
             | None -> text "none"
             | Some f -> text @@ Misc.human_duration f );
         ];
       ]

let db_prepare (db : Db.t) : _ =
  Db.exec0 db
    {|
      create table if not exists
      meta(
        key text not null unique,
        value blob
        );
      create index if not exists meta_k on meta(key);
      |}
  |> Misc.unwrap_db (fun () -> "metadata.db-prepare")

let get_meta db k : _ =
  Db.exec db {|select value from meta where key=? ;|} k
    ~ty:Db.Ty.(p1 text, p1 (nullable any_str), id)
    ~f:Db.Cursor.next
  |> Misc.unwrap_db (fun () -> spf "did not find metadata '%s'" k)
  |> Error.unwrap_opt' (fun () -> spf "expected a result for '%s'" k)

let to_db (db : Db.t) (self : t) : unit =
  let prover_versions_s =
    String.concat ","
      (List.map (fun (n, v) -> spf "%s=%s" n v) self.prover_versions)
  in
  Db.exec_no_cursor db
    "insert or replace into meta values\n\
    \    ('timestamp', ?), ('total-wall-time', ?), ('uuid', ?), \
     ('prover-versions', ?);"
    ~ty:Db.Ty.(p4 (nullable blob) (nullable blob) text text)
    (CCOpt.map string_of_float self.timestamp)
    (CCOpt.map string_of_float self.total_wall_time)
    (Uuidm.to_string self.uuid)
    prover_versions_s
  |> Misc.unwrap_db (fun () -> Fmt.asprintf "inserting metadata '%a'" pp self)

let system_keys =
  [
    "uuid"; "timestamp"; "total-wall-time"; "total_wall_time"; "prover-versions";
  ]

let is_system_key k = List.mem k system_keys

let get_all_user_meta (db : Db.t) : (string * string) list =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "metadata.get-all-user-meta" in
  Db.exec_no_params db
    {|SELECT key, value FROM meta WHERE key NOT IN ('uuid','timestamp','total-wall-time','total_wall_time','prover-versions');|}
    ~ty:Db.Ty.(p2 text (nullable any_str), fun k v -> k, v)
    ~f:Db.Cursor.to_list_rev
  |> Misc.unwrap_db (fun () -> "get-all-user-meta")
  |> List.filter_map (fun (k, v) -> CCOpt.map (fun v -> k, v) v)

let set_user_meta (db : Db.t) ~key ~value : unit =
  if is_system_key key then
    Error.failf "cannot modify system metadata key '%s'" key;
  Db.exec_no_cursor db {|INSERT OR REPLACE INTO meta(key, value) VALUES(?, ?);|}
    ~ty:Db.Ty.(p2 text text)
    key value
  |> Misc.unwrap_db (fun () -> spf "set-user-meta '%s'" key)

let delete_user_meta (db : Db.t) ~key : unit =
  if is_system_key key then
    Error.failf "cannot delete system metadata key '%s'" key;
  Db.exec_no_cursor db {|DELETE FROM meta WHERE key = ?;|}
    ~ty:Db.Ty.(p1 text)
    key
  |> Misc.unwrap_db (fun () -> spf "delete-user-meta '%s'" key)

let of_db db : t =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "metadata.of-db" in
  Error.guard (Error.wrap "while reading metadata") @@ fun () ->
  let timestamp = get_meta db "timestamp" in
  let total_wall_time = get_meta db "total-wall-time" in
  let timestamp = CCOpt.map float_of_string timestamp in
  let uuid =
    get_meta db "uuid"
    |> CCOpt.flat_map Uuidm.of_string
    |> Error.unwrap_opt "no uuid found in DB"
  in
  let total_wall_time = CCOpt.map float_of_string total_wall_time in
  let n_results =
    Db.exec_no_params_exn db "select count(*) from prover_res;"
      ~ty:Db.Ty.(p1 int, id)
      ~f:Db.Cursor.next
    |> Error.unwrap_opt "no prover results"
  in
  let n_bad = Test_analyze.of_db_n_bad db in
  let dirs = Test_analyze.of_db_dirs db in
  Logs.debug (fun k -> k "dirs: [%s]" (String.concat "," dirs));
  let provers =
    Db.exec_no_params_exn db "select name, version from prover;"
      ~f:Db.Cursor.to_list_rev
      ~ty:Db.Ty.(p2 any_str any_str, fun n v -> n, v)
  in
  let prover_names = List.map fst provers in
  {
    timestamp;
    total_wall_time;
    uuid;
    n_results;
    n_bad;
    dirs;
    provers = prover_names;
    prover_versions = provers;
  }


open Misc
open Test

type t = {
  uuid: Uuidm.t; (* unique ID *)
  timestamp: float option; (* timestamp *)
  total_wall_time: float option;
  n_results: int;
  n_bad: int;
  dirs: string list;
  provers: Prover.name list;
}

let is_complete self = CCOpt.is_some self.total_wall_time

let to_printbox ?link:(mk_link=default_linker) self : PB.t =
  let open PB in
  pb_v_record @@ List.flatten [
    ["provers", vlist_map mk_link self.provers;
     "n_results", int self.n_results; ];
    (if self.n_bad>0 then ["bad", int self.n_bad] else []);
    ["uuid", text @@ Uuidm.to_string self.uuid;
     "dirs", hlist_map text self.dirs;
     "timestamp", (match self.timestamp with
         | None -> text "none"
         | Some f -> text @@ Misc.human_datetime f);
     "total_wall_time", (match self.total_wall_time with
         | None -> text "none" | Some f -> text @@ Misc.human_duration f);
    ]
  ]

let db_prepare (db:Db.t) : _ or_error =
  Db.exec0 db {|
      create table if not exists
      meta(
        key text not null unique,
        value blob
        );
      create index if not exists meta_k on meta(key);
      |} |> Misc.db_err ~ctx:"top-res.db-prepare"

let get_meta db k : _ =
  Db.exec_exn db {|select value from meta where key=? ;|}
    k
    ~ty:Db.Ty.(p1 text, p1 (nullable any_str), id)
    ~f:Db.Cursor.next
  |> CCOpt.to_result ("did not find metadata " ^ k)

let to_db (db:Db.t) (self:t) : unit or_error =
  Db.exec_no_cursor db
    "insert or replace into meta values
    ('timestamp', ?), ('total-wall-time', ?), ('uuid', ?);"
    ~ty:Db.Ty.(p3 (nullable blob) (nullable blob) text)
    (CCOpt.map string_of_float self.timestamp)
    (CCOpt.map string_of_float self.total_wall_time)
    (Uuidm.to_string self.uuid)
  |> Misc.db_err ~ctx:"inserting metadata"

let of_db db : t or_error =
  Profile.with_ "metadata.of-db" @@ fun () ->
  Misc.err_with
    ~map_err:(Printf.sprintf "while reading metadata: %s")
    (fun scope ->
       let timestamp = get_meta db "timestamp" |> scope.unwrap in
       let total_wall_time = get_meta db "total-wall-time" |> scope.unwrap in
       let timestamp = CCOpt.map float_of_string timestamp in
       let uuid = get_meta db "uuid" |> scope.unwrap
                  |> CCOpt.flat_map Uuidm.of_string
                  |> CCOpt.to_result "no uuid found in DB"
                  |> scope.unwrap in
       let total_wall_time = CCOpt.map float_of_string total_wall_time in
       let n_results =
         Db.exec_no_params_exn db "select count(*) from prover_res;"
           ~ty:Db.Ty.(p1 int,id) ~f:Db.Cursor.next
         |> CCOpt.to_result "no prover results" |> scope.unwrap
       in
       let n_bad = Test_analyze.of_db_n_bad db |> scope.unwrap in
       let dirs = Test_analyze.of_db_dirs db |> scope.unwrap in
       Logs.debug (fun k->k "dirs: [%s]" (String.concat "," dirs));
       let provers =
         Db.exec_no_params_exn db "select distinct name from prover;"
           ~f:Db.Cursor.to_list_rev ~ty:Db.Ty.(p1 any_str, id)
       in
       { timestamp; total_wall_time; uuid; n_results; n_bad; dirs; provers; })

let pp_l out (self:t) : unit =
  Fmt.fprintf out
    "@[<v>n-results: %d%s@ provers: [%s]@ timestamp: \
     %s@ total-time: %s@ uuid: %a@]"
    self.n_results
    (if self.n_bad>0 then Printf.sprintf " bad: %d" self.n_bad else "")
    (String.concat ";" self.provers)
    (CCOpt.map_or ~default:"<no time>" Misc.human_datetime self.timestamp)
    (CCOpt.map_or ~default:"<no wall time>" Misc.human_duration self.total_wall_time)
    Uuidm.pp self.uuid

let to_string self = Fmt.asprintf "%a" pp_l self

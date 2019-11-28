module Sqlite3 = Sqlite3

module Data = Sqlite3.Data
module Rc = Sqlite3.Rc

exception Rc of Rc.t
exception Finally of exn*exn

let check_rc = function
  | Rc.OK | Rc.DONE -> ()
  | rc -> raise (Rc rc)

let bind_ stmt i (d:Data.t) : unit =
  check_rc (Sqlite3.bind stmt (i+1) d)

(** Parameters passed to a statement *)
module Ty = struct
  type (_,_) t =
    | Return : ('res, 'res) t
    | Int : ('a, 'res) t -> (int -> 'a, 'res) t
    | Int64 : ('a, 'res) t -> (int64 -> 'a, 'res) t
    | Float : ('a, 'res) t -> (float -> 'a, 'res) t
    | String : ('a, 'res) t -> (string -> 'a, 'res) t
    | Data : ('a, 'res) t -> (Data.t -> 'a, 'res) t

  let return = Return
  let int x = Int x
  let int64 x = Int64 x
  let float x = Float x
  let string x = String x
  let data x = Data x

  let rec count : type a r. (a, r) t -> int
    = function
    | Return -> 0
    | Int x -> 1 + count x
    | Int64 x -> 1 + count x
    | Float x -> 1 + count x
    | String x -> 1 + count x
    | Data x -> 1 + count x

  let rec tr_args
    : type a res. Sqlite3.stmt -> int -> (a,res) t -> (unit->res) -> a
    = fun stmt i p cb ->
      match p with
      | Return -> cb()
      | Int k ->
        (fun x -> bind_ stmt i (Data.INT (Int64.of_int x)); tr_args stmt (i+1) k cb)
      | Int64 k ->
        (fun x -> bind_ stmt i (Data.INT x); tr_args stmt (i+1) k cb)
      | String k ->
        (fun x -> bind_ stmt i (Data.TEXT x); tr_args stmt (i+1) k cb)
      | Float k ->
        (fun x -> bind_ stmt i (Data.FLOAT x); tr_args stmt (i+1) k cb)
      | Data k ->
        (fun x -> bind_ stmt i x; tr_args stmt (i+1) k cb)

  exception Bad_ty of Data.t

  (* translate results *)
  let rec tr_row
  : type a res. (int->Data.t) -> int -> (a,res) t -> a -> res
  = fun get i ty f -> match ty with
    | Return -> f
    | Data k ->
      let data = get i in
      tr_row get (i+1) k (f data)
    | Int64 k ->
      (match get i with
       | Data.INT x -> tr_row get (i+1) k (f x)
       | d -> raise (Bad_ty d))
    | Int k ->
      (match get i with
       | Data.INT x as d -> 
         let x = try Int64.to_int x with _ -> raise (Bad_ty d) in
         tr_row get (i+1) k (f x)
       | d -> raise (Bad_ty d))
    | Float k ->
      (match get i with
       | Data.FLOAT x -> tr_row get (i+1) k (f x)
       | d -> raise (Bad_ty d))
    | String k ->
      (match get i with
       | Data.BLOB x | Data.TEXT x -> tr_row get (i+1) k (f x)
       | d -> raise (Bad_ty d))
end

type statement_instance = {
  stmt : Sqlite3.stmt;
  parameter_count : int;
  mutable in_progress : bool;
}

type statement_status = New | Instantiated | Finalized

type ('a,'b,'res) statement = {
  params: ('a, 'res) Ty.t;
  res: ('b, 'res) Ty.t;
  lazy_inst : statement_instance Lazy.t;
  mutable status : statement_status;
}

let try_finally ~h y f =
  try
    let res = f () in
    h y;
    res
  with e ->
    (try h y with e' -> raise (Finally (e, e')));
    raise e

let instance st =
  if st.status = Finalized then failwith "Sqlite3EZ: attempt to use finalized statement";
  st.status <- Instantiated;
  Lazy.force st.lazy_inst

let statement_finaliser = function
  | { status = Instantiated; lazy_inst; params=_; res=_; } as st ->
    ignore (Sqlite3.finalize (Lazy.force lazy_inst).stmt);
    st.status <- Finalized
  | _ -> ()

let make_statement_ (self:Sqlite3.db) sql params res : _ statement =
  let inst =
    lazy (
      let stmt = Sqlite3.prepare self sql in
      let parameter_count = Sqlite3.bind_parameter_count stmt in
      assert (parameter_count = Ty.count params);
      {stmt; parameter_count; in_progress = false }
    ) in
  let x = { params; res; lazy_inst = inst; status = New } in
  Gc.finalise statement_finaliser x;
  x

let enter_statement_ statement =
  (* TODO handle expired/recompile *)
  let instance = instance statement in
  if instance.in_progress then (
    failwith "Sqlite3EZ: attempt to execute statement already in progress";
  );
  instance.in_progress <- true;
  instance

let exit_statement_ instance statement =
  instance.in_progress <- false;
  check_rc (Sqlite3.reset instance.stmt);
  statement.status <- statement.status; (* trick GC into keeping statement until now *)
  ()

let statement_exec (statement:('a,unit,unit) statement) : 'a =
  let instance = enter_statement_ statement in
  Ty.tr_args instance.stmt 0 statement.params
    (fun () ->
       try_finally ~h:(exit_statement_ instance) statement
         (fun () ->
           let rc = ref Rc.OK in
           while !rc <> Rc.DONE do
             rc := Sqlite3.step instance.stmt;
             if !rc = Rc.ROW then (
               failwith "Sqlite3EZ.statement_exec: not a (_ -> unit) statement";
             );
             check_rc !rc;
           done))

let statement_query_iter (type a b) ~(f:b) (statement:(a,b,unit) statement) : a =
  let instance = enter_statement_ statement in
  Ty.tr_args instance.stmt 0 statement.params
    (fun () ->
       try_finally ~h:(exit_statement_ instance) statement
         (fun () ->
            let rc = ref (Sqlite3.step instance.stmt) in
           while !rc = Rc.ROW do
             let k = Sqlite3.data_count instance.stmt in
             if k <> Ty.count statement.res then (
               failwith "Sqlite3EZ.statement_query: varying number of result columns"
             );
             Ty.tr_row (Sqlite3.column instance.stmt) 0 statement.res f;
             rc := Sqlite3.step instance.stmt;
           done;
           check_rc !rc;
           ))

let statement_query_fold (type a b res)
    ~(f:b)
    (statement:(a,b,init:res->res) statement) : a =
  let instance = enter_statement_ statement in
  Ty.tr_args instance.stmt 0 statement.params
    (fun () ->
       try_finally ~h:(exit_statement_ instance) statement
         (fun () ~init ->
            let x = ref init in
            let rc = ref (Sqlite3.step instance.stmt) in
           while !rc = Rc.ROW do
             let k = Sqlite3.data_count instance.stmt in
             if k <> Ty.count statement.res then (
               failwith "Sqlite3EZ.statement_query: varying number of result columns"
             );
             x := Ty.tr_row (Sqlite3.column instance.stmt)  0 statement.res f ~init:!x;
             rc := Sqlite3.step instance.stmt;
           done;
           check_rc !rc;
           !x
           ))

let statement_finalize x =
  if x.status = Instantiated then (
    let inst = instance x in
    check_rc (Sqlite3.finalize inst.stmt)
  );
  x.status <- Finalized

type unit_stmt_ = (unit,unit,unit) statement 

type t = {
  h : Sqlite3.db;
  mutable still_open : bool;

  statement_begin : unit_stmt_;
  statement_commit : unit_stmt_;
  statement_rollback : unit_stmt_;

  statement_savepoint : unit_stmt_;
  statement_release : unit_stmt_;
  statement_rollback_to : unit_stmt_;
}

let db_finaliser = function
  | { still_open = true; h;_} -> ignore (Sqlite3.db_close h)
  | _ -> ()

let mk_unit_stmt_ h s : unit_stmt_ =
  make_statement_ h s Ty.return Ty.return

let wrap_db h = {
  h = h;
  still_open = true;
  statement_begin = mk_unit_stmt_ h "BEGIN";
  statement_commit = mk_unit_stmt_ h "COMMIT";
  statement_rollback = mk_unit_stmt_ h "ROLLBACK";
  statement_savepoint = mk_unit_stmt_ h "SAVEPOINT a";
  statement_release = mk_unit_stmt_ h "RELEASE a";
  statement_rollback_to = mk_unit_stmt_ h "ROLLBACK TO a";
} 

let make_statement { h;_ } sql (p:_ Ty.t) r : _ statement = make_statement_ h sql p r

let open_ ?mode ?mutex ?cache ?vfs fn =
  let h = Sqlite3.db_open ?mode ?mutex ?cache ?vfs fn in
  let x = wrap_db h in
  Gc.finalise db_finaliser x;
  x

let close = function
  | x when x.still_open = true -> 
    x.still_open <- false;
    ignore (Sqlite3.db_close x.h)
  | _ -> ()

let with_open ?mode ?mutex ?cache ?vfs fn f =
  let db = open_ ?mode ?mutex ?cache ?vfs fn in
  try
    let y = f db in
    close db;
    y
  with
  | exn ->
    (try close db with exn' -> raise (Finally (exn,exn')));
    raise exn

let db_handle { h;_ } = h

let exec { h;_ } (sql:string) = check_rc (Sqlite3.exec h sql)

let exec { h;_ } (sql:string) = check_rc (Sqlite3.exec h sql)

let exec_iter {h;_} sql (ty:_ Ty.t) ~f : unit =
  let rc =
    Sqlite3.exec_no_headers
      ~cb:(fun row ->
          Ty.tr_row (Array.get row) 0 ty f)
    h sql in
  check_rc rc

let exec_fold {h;_} sql (ty:_ Ty.t) ~f ~init =
  let res = ref init in
  let rc =
    Sqlite3.exec_no_headers
      ~cb:(fun row ->
          res := Ty.tr_row (Sqlite3. Array.get row) 0 ty f !res)
    h sql in
  check_rc rc;
  !res

let transact db f =
  statement_exec db.statement_begin;
  try
    let y = f db in
    statement_exec db.statement_commit;
    y
  with
  | exn ->
    (try statement_exec db.statement_rollback with exn' -> raise (Finally (exn,exn')));
    raise exn

let atomically db f =
  statement_exec db.statement_savepoint;
  try
    let y = f db in
    statement_exec db.statement_release;
    y
  with
  | exn ->
    (try
       statement_exec db.statement_rollback_to;
       statement_exec db.statement_release;
     with exn' -> raise (Finally (exn,exn'))
    );
    raise exn

let last_insert_rowid db = Sqlite3.last_insert_rowid db.h

let changes db = Sqlite3.changes db.h

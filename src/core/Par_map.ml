(* This file is free software. See file "license" for more details. *)

(** Parallel execution utilities *)

module Log = (val Logs.src_log (Logs.Src.create "benchpress.par_map"))

let die_on_sigterm : unit -> unit =
  let thunk =
    lazy
      (Sys.set_signal 15
         (Sys.Signal_handle
            (fun _ ->
              print_endline "received sigterm, exiting";
              Unix.kill 0 15;
              (* kill children *)
              exit 1)))
  in
  fun () -> Lazy.force thunk

(** Map on the list with at most [j] parallel threads *)
let map_p ~j f l =
  if j < 1 then invalid_arg "map_p: ~j";
  die_on_sigterm ();
  match l with
  | [] -> []
  | _ ->
    Log.debug (fun k -> k "par-map: create pool j=%d" j);
    let pool = Moonpool.Fifo_pool.create ~num_threads:j () in
    let res =
      try
        let futs =
          List.map (fun x -> Moonpool.Fut.spawn ~on:pool (fun () -> f x)) l
        in
        let results = List.map Moonpool.Fut.wait_block futs in
        List.map
          (function
            | Ok x -> x
            | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt)
          results
      with e ->
        Log.debug (fun k -> k "par-map: shutdown pool (exception)");
        Moonpool.Fifo_pool.shutdown pool;
        raise e
    in
    Log.debug (fun k -> k "par-map: shutdown pool");
    Moonpool.Fifo_pool.shutdown pool;
    res

(** Map on the list [l] with each call to [f] being associated one of the
    resources from [resources] that is guaranteed not to be used concurrently
    by another call to [f]. *)
let map_with_resource ~resources f l =
  match l with
  | [] -> []
  | _ ->
    if CCList.is_empty resources then
      invalid_arg "map_with_resource: ~resources";
    die_on_sigterm ();
    let jobs = List.length resources in
    let queue = Moonpool.Blocking_queue.create () in
    List.iter (Moonpool.Blocking_queue.push queue) resources;
    let f_with_resource x =
      let resource = Moonpool.Blocking_queue.pop queue in
      Fun.protect
        ~finally:(fun () -> Moonpool.Blocking_queue.push queue resource)
        (fun () -> f resource x)
    in
    Log.debug (fun m -> m "par-map: create pool j=%d" jobs);
    let pool = Moonpool.Fifo_pool.create ~num_threads:jobs () in
    let res =
      try
        let futs =
          List.map
            (fun x ->
              Moonpool.Fut.spawn ~on:pool (fun () -> f_with_resource x))
            l
        in
        let results = List.map Moonpool.Fut.wait_block futs in
        Moonpool.Blocking_queue.close queue;
        List.map
          (function
            | Ok x -> x
            | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt)
          results
      with e ->
        Log.debug (fun m -> m "par-map: shutdown pool (exception)");
        Moonpool.Blocking_queue.close queue;
        Moonpool.Fifo_pool.shutdown pool;
        raise e
    in
    Log.debug (fun m -> m "par-map: shutdown pool");
    Moonpool.Fifo_pool.shutdown pool;
    res

let synchronized f =
  let lock = Moonpool.Lock.create () in
  Moonpool.Lock.with_ lock f

let with_affinity cpu f =
  let aff = Processor.Affinity.get_ids () in
  Processor.Affinity.set_ids [ cpu ];
  Fun.protect ~finally:(fun () -> Processor.Affinity.set_ids aff) f

let with_affinity_opt cpu f =
  match cpu with
  | None -> f ()
  | Some cpu -> with_affinity cpu f

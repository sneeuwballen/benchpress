(* This file is free software. See file "license" for more details. *)

(** Parallel execution utilities *)

open Common
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

(** Map on the list with at most [j] parallel fibers *)
let map_p ~j f l =
  if j < 1 then invalid_arg "map_p: ~j";
  die_on_sigterm ();
  match l with
  | [] -> []
  | _ when j = 1 -> List.map f l
  | _ ->
    Log.debug (fun k -> k "par-map: start j=%d n=%d" j (List.length l));
    let sem = Eio.Semaphore.make j in
    let promises =
      let@ sw = Eio.Switch.run ~name:"parmap" in
      List.rev_map
        (fun x ->
          let@ () = Eio.Fiber.fork_promise ~sw in
          Eio.Semaphore.acquire sem;
          let@ () =
            Fun.protect ~finally:(fun () -> Eio.Semaphore.release sem)
          in
          f x)
        l
    in
    let res = List.rev_map Eio.Promise.await_exn promises in
    Log.debug (fun k -> k "par-map: done");
    res

(** Map on the list [l] with each call to [f] being associated one of the
    resources from [resources] that is guaranteed not to be used concurrently by
    another call to [f]. *)
let map_with_resource ~resources f l =
  match l with
  | [] -> []
  | _ ->
    if CCList.is_empty resources then
      invalid_arg "map_with_resource: ~resources";
    die_on_sigterm ();
    let n = List.length l in
    let results = Array.make n None in
    let queue = Eio.Stream.create (List.length resources) in
    List.iter (Eio.Stream.add queue) resources;
    let f_with_resource i x =
      let resource = Eio.Stream.take queue in
      let@ () =
        Fun.protect ~finally:(fun () -> Eio.Stream.add queue resource)
      in
      results.(i) <- Some (f resource x)
    in
    Log.debug (fun m -> m "par-map: start n=%d" n);
    Eio.Switch.run (fun sw ->
        List.iteri
          (fun i x -> Eio.Fiber.fork ~sw (fun () -> f_with_resource i x))
          l);
    Log.debug (fun m -> m "par-map: done");
    Array.to_list results |> List.map (fun x -> Option.get x)

let with_affinity cpu f =
  let aff = Processor.Affinity.get_ids () in
  Processor.Affinity.set_ids [ cpu ];
  Fun.protect ~finally:(fun () -> Processor.Affinity.set_ids aff) f

let with_affinity_opt cpu f =
  match cpu with
  | None -> f ()
  | Some cpu -> with_affinity cpu f

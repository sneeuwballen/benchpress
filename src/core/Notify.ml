
(* This file is free software. See file "license" for more details. *)

(** {1 Notifications} *)

type t = {
  notify: string -> unit;
  sync: unit -> unit;
}

let all_ : (Config.t -> t option) list ref = ref []

module Internal = struct
  let mk_ ~notify ~sync : t = {notify;sync}
  let register f = all_ := f :: !all_
end

open Internal

let send (n:t) s = n.notify s
let sendf (n:t) fmt = CCFormat.ksprintf ~f:n.notify fmt
let sync (n:t): unit = n.sync()

let nil : t = mk_ ~notify:(fun _ ->()) ~sync:(fun _ -> ())

let stdout : t = mk_ ~notify:print_endline ~sync:(fun _ -> flush stdout)

let combine a b : t =
  mk_ ~notify:(fun x -> a.notify x; b.notify x) ~sync:(fun () -> a.sync(); b.sync())

let combine_l l : t =
  mk_
    ~notify:(fun x -> List.iter (fun n -> send n x) l)
    ~sync:(fun() -> List.iter (fun n->n.sync()) l)

let load_findlib_ =
  let th = lazy (Findlib.init()) in
  fun () -> Lazy.force th

let try_load ~lib ~file =
  try
    load_findlib_ ();
    let d = Findlib.package_directory lib in
    let path = Filename.concat d (file ^ if Sys.backend_type=Sys.Native then ".cmxs" else ".cma") in
    Printf.eprintf "try to load plugin %s: path %S\n%!" file path;
    Dynlink.loadfile path
  with
  | Dynlink.Error e ->
    Printf.eprintf "error when trying to load plugin %S:\n%s\n%!" file (Dynlink.error_message e)
  | e ->
    Printf.eprintf "error when trying to load plugin %S:\n%s\n%!" file (Printexc.to_string e)

let make (config:Config.t) : t =
  let l = CCList.filter_map (fun f -> f config) !all_ in
  combine_l (stdout :: l)


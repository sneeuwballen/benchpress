(* This file is free software. See file "license" for more details. *)

(** {1 Notifications} *)

type t = {
  notify: string -> unit;
  sync: unit -> unit;
}

let mk_ notify sync : t = {notify;sync}

let send (n:t) s = n.notify s
let sendf (n:t) fmt = CCFormat.ksprintf ~f:n.notify fmt
let sync (n:t): unit = n.sync()

let nil : t = mk_ (fun _ ->()) (fun _ -> ())

let stdout : t =
  mk_ (fun s -> output_string stdout Misc.reset_line; print_endline s)
    (fun _ -> flush stdout)

let combine a b : t =
  mk_ (fun x -> a.notify x; b.notify x) (fun () -> a.sync(); b.sync())

let combine_l l : t =
  mk_
    (fun x -> List.iter (fun n -> send n x) l)
    (fun() -> List.iter (fun n->n.sync()) l)

(* TODO: run subprocess if IRC flag activated (assumings deps are there
   and the irc notifier program is installed), send it messages on stdin 

   OR: sub-library, that we can try to dynlink optionally
  *)

(* TODO: stanza for IRC *)
let make (_defs) : t =
  (* TODO: notification by IRC, if any *)
  combine_l [stdout]


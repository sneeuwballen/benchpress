let name_of_project = ref "benchpress"

let getenv_or_empty s =
  try Sys.getenv s with _ -> ""

let (<+>) x y = if x="" then y() else x

let get_home : unit -> string =
  let s = lazy (getenv_or_empty "HOME" <+> (fun () -> "/tmp")) in
  fun () -> Lazy.force s

let interpolate_home ?(f=fun _-> None) s =
  let buf = Buffer.create (String.length s) in
  Buffer.add_substitute buf
    (function
      | "HOME" | "home" -> get_home()
      | s ->
        begin match f s with
          | Some u -> u
          | None ->
            Error.failf "interpolate home: couldn't find variable: '%s'" s
        end)
    s;
  Buffer.contents buf

let config_dir () =
  getenv_or_empty "XDG_CONFIG_HOME" <+> (fun () -> get_home() ^ "/.config")

let data_dir () =
  getenv_or_empty "XDG_DATA_HOME" <+> (fun () -> get_home() ^ "/.local/share/")

let cache_dir () =
  getenv_or_empty "XDG_CACHE_HOME" <+> (fun () -> get_home() ^ "/.cache/")

let runtime_dir () =
  getenv_or_empty "XDG_RUNTIME_DIR" <+> (fun () -> get_home() ^ "/tmp/")

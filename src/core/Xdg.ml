let name_of_project = ref "logitest"

let getenv_or_empty s =
  try Sys.getenv s with _ -> ""

let (<+>) x y = if x="" then y() else x

let get_home () : string = getenv_or_empty "HOME" <+> (fun () -> "/tmp")

let config_dir () =
  getenv_or_empty "XDG_CONFIG_HOME" <+> (fun () -> get_home() ^ "/.config")

let data_dir () =
  getenv_or_empty "XDG_DATA_HOME" <+> (fun () -> get_home() ^ "/.local/share/")

let cache_dir () =
  getenv_or_empty "XDG_CACHE_HOME" <+> (fun () -> get_home() ^ "/.cache/")

let runtime_dir () =
  getenv_or_empty "XDG_RUNTIME_DIR" <+> (fun () -> get_home() ^ "/tmp/")

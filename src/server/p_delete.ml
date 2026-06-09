open Common
module H = Tiny_httpd
module U = Tiny_httpd.Util
module PB = PrintBox
module Log = (val Logs.src_log (Logs.Src.create "benchpress-serve"))

let handle_delete (self : Server_common.t) : unit =
  let open Server_common in
  assert self.allow_delete;
  let run names =
    Log.debug (fun k -> k "/delete1: names is [%s]" @@ String.concat ";" names);
    let files =
      names
      |> CCList.map (fun s ->
             match Bin_utils.mk_file_full s with
             | exception Error.E e ->
               Log.err (fun k -> k "cannot load file %S: %a" s Error.pp e);
               H.Response.fail_raise ~code:404 "invalid file %S: %s" s
               @@ Error.show e
             | x -> x)
    in
    List.iter
      (fun file ->
        Log.info (fun k -> k "delete file %s" @@ Filename.quote file);
        Sys.remove file)
      files;
    (* return empty html *)
    H.Response.make_string ~headers:default_html_headers (Ok "")
  in
  H.add_route_handler self.server ~meth:`DELETE
    ~middlewares:
      [
        Auth_middleware.middleware ~allow_localhost:self.allow_localhost
          self.auth;
      ]
    H.Route.(exact "delete1" @/ string_urlencoded @/ return)
    (fun file _req ->
      Log.debug (fun k -> k "/delete1: path is %s" file);
      run [ file ]);
  ()

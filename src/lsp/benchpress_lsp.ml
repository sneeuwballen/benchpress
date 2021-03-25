
module Sexp_loc = Benchpress.Sexp_loc
module Stanza = Benchpress.Stanza

module Lock = CCLock

module IO = Jsonrpc2.Blocking_IO
module L = Jsonrpc2.Make(IO)
type loc = Sexp_loc.loc

type processed_buf = (Stanza.t list, string * Sexp_loc.loc) result

let range_of_loc_ (l:loc) : Lsp.Types.Range.t =
  let mk_pos_ p =
    Lsp.Types.Position.create ~line:(1+p.Sexp_loc.line) ~character:p.col in
  Lsp.Types.Range.create ~start:(mk_pos_ l.start) ~end_:(mk_pos_ l.stop)

let diagnostics (p:processed_buf) : _ list =
  match p with
  | Ok _ -> []
  | Error (msg,loc) ->
    let d =
      Lsp.Types.(Diagnostic.create ~severity:DiagnosticSeverity.Error
        ~range:(range_of_loc_ loc) ~message:msg ()) in
    [d]

class blsp = object(self)
  inherit L.server

  (* one env per document *)
  val buffers: (Lsp.Types.DocumentUri.t, processed_buf) Hashtbl.t Lock.t
    = Lock.create @@ Hashtbl.create 32

  (* We define here a helper method that will:
     - process a document
     - store the state resulting from the processing
     - return the diagnostics from the new state
  *)
  method private _on_doc
      ~(notify_back:L.notify_back)
      (uri:Lsp.Types.DocumentUri.t) (contents:string) =
    Format.eprintf "on doc@.";
    let r = Stanza.parse_string ~filename:uri contents in
    Lock.with_lock buffers (fun b -> Hashtbl.replace b uri r);
    let diags = diagnostics r in
    notify_back#send_diagnostic diags

  (* We now override the [on_notify_doc_did_open] method that will be called
     by the server each time a new document is opened. *)
  method on_notif_doc_did_open ~notify_back d ~content : unit IO.t =
    self#_on_doc ~notify_back d.uri content

  (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
     by the server each time a new document is opened. *)
  method on_notif_doc_did_change ~notify_back d _c ~old_content:_old ~new_content =
    self#_on_doc ~notify_back d.uri new_content

  (* On document closes, we remove the state associated to the file from the global
     hashtable state, to avoid leaking memory. *)
  method on_notif_doc_did_close ~notify_back:_ d : unit IO.t =
    Lock.with_lock buffers (fun b -> Hashtbl.remove b d.uri);
    IO.return ()
end

let () =
  let lsp = new blsp in
  let server = L.create ~ic:stdin ~oc:stdout lsp in
  L.run server

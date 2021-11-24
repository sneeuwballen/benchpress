
module Loc = Benchpress.Loc
module Stanza = Benchpress.Stanza
module Or_error = Benchpress.Or_error
module Error = Benchpress.Error

module Lock = CCLock

module IO = Linol.Blocking_IO
module L = Linol.Jsonrpc2.Make(IO)
module LT = Lsp.Types
module Log = (val Logs.src_log Logs.Src.(create "lsp"))

type loc = Loc.t

type processed_buf = {
  stanzas: Stanza.t list Or_error.t;
}

let range_of_loc_ (l:loc) : Lsp.Types.Range.t =
  let mk_pos_ p =
    Lsp.Types.Position.create ~line:(p.Loc.line-1) ~character:p.col in
  Lsp.Types.Range.create ~start:(mk_pos_ l.start) ~end_:(mk_pos_ l.stop)

let diagnostics ~uri (p:processed_buf) : _ list =
  match p.stanzas with
  | Ok _l ->
    Log.debug (fun k->k"in %s: ok, %d stanzas" uri (List.length _l));
    []
  | Error e0 ->
    Log.debug (fun k->k"in %s: err %s" uri (Error.show e0));
    let e, ctx = Error.unwrap_ctx e0 in

    let errs =
      (e :: ctx)
      |> CCList.filter_map
        (fun e -> match Error.loc e with
           | None -> None
           |Some loc -> Some (Error.msg e, loc))
    in

    let tr_ctx_err_ (msg,loc) =
      LT.DiagnosticRelatedInformation.create
        ~location:(LT.Location.create ~uri ~range:(range_of_loc_ loc))
        ~message:msg
    in

    begin match errs with
      | [] ->
        Log.err (fun k->k"in %s: err with no loc:@ %a" uri Error.pp e0);
        []
      | (msg0, loc0) :: ctx_errs ->
        let d =
          LT.Diagnostic.create ~severity:LT.DiagnosticSeverity.Error
            ~range:(range_of_loc_ loc0) ~message:msg0
            ~relatedInformation:(List.map tr_ctx_err_ ctx_errs)
            ()
        in
        [d]
    end

class blsp = object(self)
  inherit L.server

  (* one env per document *)
  val buffers: (Lsp.Types.DocumentUri.t, processed_buf) Hashtbl.t Lock.t
    = Lock.create @@ Hashtbl.create 32

  method! config_hover = Some (`Bool false)
  method! config_definition = Some (`Bool false)

  (* configure how sync happens *)
  method! config_sync_opts =
    let change = LT.TextDocumentSyncKind.Incremental in
    LT.TextDocumentSyncOptions.create ~openClose:true ~change
      ~save:(LT.SaveOptions.create ~includeText:false ())
      ()

  (* We define here a helper method that will:
     - process a document
     - store the state resulting from the processing
     - return the diagnostics from the new state
  *)
  method private _on_doc
      ~(notify_back:L.notify_back)
      (uri:Lsp.Types.DocumentUri.t) (contents:string) =
    Log.debug (fun k->k"on doc %s" uri);
    let r = Stanza.parse_string ~filename:uri contents in
    let pdoc = {stanzas=r} in
    Lock.with_lock buffers (fun b -> Hashtbl.replace b uri pdoc);
    let diags = diagnostics ~uri pdoc in
    Log.debug (fun k->k"send diags for %s" uri);
    notify_back#send_diagnostic diags

  (* We now override the [on_notify_doc_did_open] method that will be called
     by the server each time a new document is opened. *)
  method on_notif_doc_did_open ~notify_back (d:LT.TextDocumentItem.t) ~content : unit IO.t =
    self#_on_doc ~notify_back d.uri content

  (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
     by the server each time a new document is opened. *)
  method on_notif_doc_did_change ~notify_back
      (d:LT.VersionedTextDocumentIdentifier.t) _c ~old_content:_old ~new_content =
    self#_on_doc ~notify_back d.uri new_content

  (* On document closes, we remove the state associated to the file from the global
     hashtable state, to avoid leaking memory. *)
  method on_notif_doc_did_close ~notify_back:_
      (d:LT.TextDocumentIdentifier.t) : unit IO.t =
    Log.debug (fun k->k"close %s" d.uri);
    Lock.with_lock buffers (fun b -> Hashtbl.remove b d.uri);
    IO.return ()
end

let setup_debug() =
  if Sys.getenv_opt "LSP_DEBUG"=Some "1" then (
    let oc = open_out "/tmp/lsp.log" in
    let out = Format.formatter_of_out_channel oc in
    at_exit (fun () -> Format.fprintf out "@?"; close_out oc);
    Logs.set_reporter (Logs.format_reporter ~app:out ~dst:out ());
    Logs.set_level ~all:true (Some Logs.Debug);
    Log.info (fun k->k"started LSP");
  )

let () =
  setup_debug();
  let lsp = new blsp in
  let server = L.create ~ic:stdin ~oc:stdout lsp in
  L.run server

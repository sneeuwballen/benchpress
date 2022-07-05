
module Loc = Benchpress.Loc
module Stanza = Benchpress.Stanza
module Error = Benchpress.Error
module Definitions = Benchpress.Definitions
module Sexp_loc = Benchpress.Sexp_loc

module Lock = CCLock

type 'a or_error = ('a, Error.t) result
module E = CCResult

module IO = Linol.Blocking_IO
module L = Linol.Jsonrpc2.Make(IO)
module LT = Lsp.Types
module Log = (val Logs.src_log Logs.Src.(create "lsp"))

type loc = Loc.t

type processed_buf = {
  text: string;
  stanzas: Stanza.t list or_error;
  defs: Definitions.t or_error;
}

let range_of_loc_ (l:loc) : Lsp.Types.Range.t =
  let mk_pos_ p =
    let line, col = Loc.Pos.to_line_col l.input p in
    Lsp.Types.Position.create ~line:(line - 1) ~character:col in
  Lsp.Types.Range.create ~start:(mk_pos_ l.start) ~end_:(mk_pos_ l.stop)

let diag_of_error ~uri (e0:Error.t) : LT.Diagnostic.t list =
  let e, ctx = Error.unwrap_ctx e0 in
  let errs =
    (e :: ctx)
    |> CCList.filter_map
      (fun e -> match Error.loc e with
         | None -> None
         | Some loc -> Some (Error.msg e, loc))
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

let diagnostics ~uri (p:processed_buf) : _ list =
  match p.stanzas with
  | Ok l ->
    (* parsed succesfully, but stanzas might contain some additional errors *)
    let defs_errors = match p.defs with
      | Ok defs -> Definitions.errors defs
      | Error e -> [e]
    in
    let errors = List.rev_append (Stanza.errors l) defs_errors in
    Log.debug (fun k->k"in %s: ok, %d stanzas, %d errors" uri (List.length l) (List.length errors));
    CCList.flat_map (diag_of_error ~uri) errors

  | Error e0 ->
    diag_of_error ~uri e0

let find_atom_under_ (s:string) (pos:Loc.pos) : string option =
  let buf = Lexing.from_string ~with_positions:true s in
  let input = Loc.Input.string s in
  let exception E of string in
  try
    while true do
      let tok = CCSexp_lex.token buf in
      let loc = Loc.of_lexbuf ~input buf in
      if Loc.contains loc pos then (
        match tok with
        | CCSexp_lex.ATOM s -> raise (E s)
        | _ -> raise Exit
      ) else if Loc.Pos.le input pos loc.start then (
        raise Exit
      )
    done;
    assert false
  with
  | Exit -> None
  | E s -> Some s
;;

let catch_e f =
  try Ok (f())
  with Error.E e -> Error e

class blsp = object(self)
  inherit L.server

  (* one env per document *)
  val buffers: (Lsp.Types.DocumentUri.t, processed_buf) Hashtbl.t Lock.t
    = Lock.create @@ Hashtbl.create 32

  method! config_hover = Some (`Bool true)
  method! config_definition = Some (`Bool true)
  method! config_completion =
    let opts = LT.CompletionOptions.create () in
    Some opts

  (* configure how sync happens *)
  method! config_sync_opts =
    let change = LT.TextDocumentSyncKind.Incremental in
    LT.TextDocumentSyncOptions.create ~openClose:true ~change
      ~save:(LT.SaveOptions.create ~includeText:false ())
      ()

  method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos
      (_ : L.doc_state) : LT.Hover.t option =
    let pos = Loc.Pos.of_line_col pos.L.Position.line pos.character in
    begin
      match Lock.with_lock buffers (fun b -> CCHashtbl.get b uri) with
      | Some {defs=Ok defs; text; _} ->
        Log.debug (fun k->k"found buffer with defs");
        begin match find_atom_under_ text pos with
          | None -> None
          | Some a ->
            begin match Definitions.find defs a with
              | None ->
                Log.err (fun k->k"didn't find def for %S" a);
                None

              | Some d ->
                let contents =
                  `MarkedString {LT.MarkedString.value=Definitions.Def.show d; language=None}
                in
                let h = LT.Hover.create ~contents () in
                Some h
            end
        end
      | _ -> None
    end

  method! on_req_definition ~notify_back:_ ~id:_ ~uri ~pos
      (_ : L.doc_state) : LT.Locations.t option =
    let pos = Loc.Pos.of_line_col pos.L.Position.line pos.character in
    begin
      match Lock.with_lock buffers (fun b -> CCHashtbl.get b uri) with
      | Some {defs=Ok defs; text; _} ->
        Log.debug (fun k->k"found buffer with defs");
        begin match find_atom_under_ text pos with
          | None -> None
          | Some a ->
            begin match Definitions.find defs a with
              | None ->
                Log.err (fun k->k"didn't find def for %S" a);
                None

              | Some d ->
                let loc = Definitions.Def.loc d in
                let range = range_of_loc_ loc in
                (* FIXME: find the right URI, def could come from another file *)
                let l = `Location [LT.Location.create ~uri ~range] in
                Some l
            end
        end
      | _ -> None
    end

  (* FIXME: completion is never useful on a parsable buffer, and
     buffers that do not parse have no definitions *)
  method! on_req_completion ~notify_back:_ ~id:_ ~uri ~pos ~ctx:_
        (_ : L.doc_state) :
          [ `CompletionList of LT.CompletionList.t
          | `List of LT.CompletionItem.t list ] option =
    Log.debug (
      fun k->k"completion request in '%s' at pos: %d line, %d col"
          uri pos.line pos.character);
    let pos = Loc.Pos.of_line_col pos.line pos.character in
    begin
      match Lock.with_lock buffers (fun b -> CCHashtbl.get b uri) with
      | Some {defs=Ok defs; text; _} ->
        Log.debug (fun k->k"found local doc");
        begin match find_atom_under_ text pos with
          | None -> None
          | Some a ->
            Log.debug (fun k->k"found atom %S" a);
            let l =
              Definitions.completions ~before_pos:pos defs a
              |> List.map
                (fun d ->
                   let name, label = match d with
                     | Definitions.D_prover p -> p.view.name, "prover"
                     | Definitions.D_task t -> t.view.name , "task"
                     | Definitions.D_proof_checker t -> t.view.name , "proof-checker"
                   in
                   LT.CompletionItem.create ~label
                     ~detail:(Definitions.Def.show d)
                     ~insertText:name ()
                )
            in
            Some (`List l)
        end
      | _ -> None
    end

  (* We define here a helper method that will:
     - process a document
     - store the state resulting from the processing
     - return the diagnostics from the new state
  *)
  method private _on_doc
      ~(notify_back:L.notify_back)
      (uri:Lsp.Types.DocumentUri.t) (contents:string) =
    Log.debug (fun k->k"on doc %s" uri);
    let open E.Infix in
    let stanzas =
      catch_e @@ fun () ->
      Stanza.parse_string ~reify_errors:true ~filename:uri contents
    in
    let defs =
      let* stanzas = stanzas in
      catch_e @@ fun () ->
      Definitions.of_stanza_l ~reify_errors:true stanzas
    in
    let pdoc = {stanzas; defs; text=contents} in
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

(** Ambient span provider for Trace using Eio fiber-local storage *)

open Trace_core

open struct
  let k_span : span Eio.Fiber.key = Eio.Fiber.create_key ()
  let get_current_span () = Eio.Fiber.get k_span

  let with_current_span_set_to () span f =
    Eio.Fiber.with_binding k_span span (fun () -> f span)

  let callbacks : unit Ambient_span_provider.Callbacks.t =
    { get_current_span; with_current_span_set_to }
end

let provider : Ambient_span_provider.t = ASP_some ((), callbacks)
let setup () = Trace_core.set_ambient_context_provider provider

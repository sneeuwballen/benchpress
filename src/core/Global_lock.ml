open struct
  let _eio_lock = Eio.Mutex.create ()
  let _lock = Mutex.create ()
end

let synchronized_eio f = Eio.Mutex.use_ro _eio_lock f

let synchronized_sync f =
  Mutex.lock _lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock _lock) f

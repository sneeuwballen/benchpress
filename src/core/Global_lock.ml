open struct
  let _lock = Eio.Mutex.create ()
end

let synchronized f = Eio.Mutex.use_ro _lock f

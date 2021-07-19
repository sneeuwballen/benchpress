
; read smtlib status
(prover
  (name smtlib-read-status)
  (cmd "grep :status $file")
  (unknown ":status unknown")
  (sat ":status sat")
  (unsat ":status unsat"))

(prover
  (name minisat)
  (unsat "UNSATISFIABLE")
  (sat "^SATISFIABLE")
  (cmd "minisat -cpu-lim=$timeout $file"))

(prover
  (name z3)
  (cmd "z3 $file")
  (version "cmd:z3 --version")
  (unsat "unsat|(s UNSATISFIABLE)")
  (sat "(^sat)|(s SATISFIABLE)"))

(prover
  (name cvc4)
  (cmd "cvc4 $file ")
  (version "cmd:cvc4 --version | head -n 3")
  (unsat "unsat")
  (sat "^sat"))

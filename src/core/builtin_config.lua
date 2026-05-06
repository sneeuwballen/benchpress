benchpress.prover {
  name    = "smtlib-read-status",
  cmd     = "grep :status $file",
  unknown = ":status unknown",
  sat     = ":status sat",
  unsat   = ":status unsat",
}

benchpress.prover {
  name  = "minisat",
  unsat = "UNSATISFIABLE",
  sat   = "^SATISFIABLE",
  cmd   = "minisat -cpu-lim=$timeout $file",
}

benchpress.prover {
  name  = "z3",
  cmd   = "z3 $file",
  unsat = "unsat|(s UNSATISFIABLE)",
  sat   = "(^sat)|(s SATISFIABLE)",
}

benchpress.prover {
  name  = "cvc4",
  cmd   = "cvc4 $file",
  unsat = "unsat",
  sat   = "^sat",
}

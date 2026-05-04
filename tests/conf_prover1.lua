-- Lua equivalent of conf_prover1.sexp

benchpress.prover {
  name   = "templ1",
  cmd    = "$binary $file",
  sat    = "^SAT",
  unsat  = "^UNSAT",
  unknown = "^UNKNOWN",
}

benchpress.prover {
  name   = "fake1",
  binary = "$cur_dir/fake_prover.sh",
  cmd    = "$binary $file",
  sat    = "^SAT",
  unsat  = "^UNSAT",
  unknown = "^UNKNOWN",
}

benchpress.dir {
  name    = "fake",
  path    = "$cur_dir/fake_files/",
  pattern = ".*.fake",
}

benchpress.task {
  name   = "runall",
  action = benchpress.run_provers {
    provers = { "fake1" },
    dirs    = { "$cur_dir/fake_files/" },
    timeout = 30,
  },
}

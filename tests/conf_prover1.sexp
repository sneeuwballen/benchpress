
(import-prelude false)

(prover
  (name templ1)
  (cmd "$binary $file")
  (sat "^SAT")
  (unsat "^UNSAT")
  (unknown "^UNKNOWN"))

(proof_checker
  (name gadget)
  (cmd "$cur_dir/fake_checker.sh $file $proof_file")
  (valid "^VALID")
  (invalid "^INVALID"))

(prover
  (name templ2)
  (cmd "$binary $file $proof_file")
  (proof_ext "notreallyaproof")
  (produces_proof true)
  (proof_checker gadget)
  (inherits templ1))

(prover
  (name fake1)
  (binary "$cur_dir/fake_prover.sh") ; yep, pretty fake
  (inherits templ1))

(prover
  (name fake2)
  (binary "$cur_dir/fake_prover.sh")
  (inherits templ2))

(dir
  (name fake)
  (pattern ".*.fake")
  (path $cur_dir/fake_files/))

(task
  (name runall)
  (action
    (run_provers
      (provers (fake1 fake2))
      (dirs (${dir:fake})))))

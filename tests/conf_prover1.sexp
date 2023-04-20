
(import-prelude false)

(prover
  (cmd "$cur_dir/fake_prover.sh $file") ; yep, pretty fake
  (sat "^SAT")
  (unsat "^UNSAT")
  (unknown "^UNKNOWN")
  (name fake1))

(proof_checker
  (name gadget)
  (cmd "$cur_dir/fake_checker.sh $file $proof_file")
  (valid "^VALID")
  (invalid "^INVALID"))

(prover
  (name fake2)
  (cmd "$cur_dir/fake_prover.sh $file $proof_file")
  (proof_ext "notreallyaproof")
  (produces_proof true)
  (proof_checker gadget)
  (inherits fake1))

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

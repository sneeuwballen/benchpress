
(import-prelude)

(prover
  (name sidekick)
  (version "cmd:sidekick --version")
  (cmd "sidekick --no-check --time $timeout $file")
  (unsat "Unsat")
  (sat "Sat")
  (unknown "Timeout|Unknown"))

(prover
  (name mc2)
  (cmd "ulimit -t $timeout; mc2 --time $timeout $file")
  (unsat "^Unsat")
  (sat "^Sat")
  (unknown "Unknown")
  (timeout "Timeout"))

(dir
  (path "$HOME/w/smtlib")
  (pattern ".*.smt2")
  (expect (run smtlib-read-status)))

(task
  (name glob-all-smtlib)
  (synopsis "run all SMT solvers on smtlib")
  (action
   (run_provers
    (dirs "$HOME/w/smtlib")
    (provers (mc2 z3 sidekick))
    ;(memory 100000000)  ; TODO: parse "10G"
    (timeout 10))))

(task
  (name glob-all-smtlib-QF_UF)
  (synopsis "run all SMT solvers on QF_UF")
  (action
    (run_provers
      (dirs "$HOME/w/smtlib/QF_UF")
      (provers (mc2 z3 sidekick))
      (timeout 10))))

(task
  (name glob-all-nodir)
  (synopsis "run all SMT solvers on provided dir")
  (action
   (run_provers
    (dirs ())
    (provers (mc2 z3 sidekick))
    (timeout 10))))

(task
  (name sleep-20)
  (synopsis "sleep for 20s (debug)")
  (action
   (run_cmd "sleep 20")))

; Custom tags
(custom-tag (name ok))
(custom-tag (name warn))
(custom-tag (name fatal))
(custom-tag (name memout))
(custom-tag (name timout))
(custom-tag (name overflow))

; dolmen
(prover
  (name dolmen)
  ; ${time} is in seconds
  ; ${memory} is in M
  (cmd "dolmen -b --context --strict=false --color=false -t ${{time}}s -s ${{memory}}M ${file}")
  ; order is important here, as the regexps are checked
  ; in the order defined, and given that warnings and errors
  ; are printed on stderr, stdout will always match the
  ; regexp for the ok tag....
  (tags
    ((timout "Time limit reached|Out_of_time")
     (memout "Memory limit reached|Out_of_space")
     (overflow "Stack overflow")
     (warn "Warning")
     (fatal "Error|Killed")
     (ok "^$")))
)


; vim:ft=benchpress:

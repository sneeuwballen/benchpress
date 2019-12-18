# Benchpress

[![build status](https://travis-ci.org/sneeuwballen/benchpress.svg?branch=master "build status")](https://travis-ci.org/sneeuwballen/benchpress)

Tool to run one or more logic programs, on a set of files, and collect the
results.

License: BSD.

## Basic usage

```sh
$ benchpress run -c foo.sexp dir_a/ dir_b/ -p z3
…
```

this tells `benchpress` to run the prover `z3` on directories `dir_a`
and `dir_b`. `foo.sexp` contains additional configuration parameters
as described below.

## System dependencies

Logitest relies on a bunch of utilities, besides OCaml libraries:

- `sqlite3` (with development headers)
- `time`, `ulimit`, etc
- ~~`gzip`/`zcat` for compressing files~~
- (optional) `grep` + access to `/proc/cpuinfo` for guessing number of cores
- (optional) `git` for tagging solvers from their repository

**TODO** use cgroups or similar for CPU affinity

## Options

Most of the commands accept `-c <config file>` to specify which config files to use.

- `benchpress --help` to list options
- `benchpress run`:
  * `-t <time>` timeout (in seconds) for each run
  * `-m <memory>` memory limit in MB
  * `-F <file>` read list of problems from given file
  * `-p <prover1,prover2>` list of provers to use
  * `--task <profile>` specify which task to use
- `benchpress dir config` shows the configuration directory
- `benchpress dir state` shows the directory where the state (benchmark results) is stored
- `benchpress check-config <file>` to check that the file is valid configuration
- `benchpress prover-list` to list available provers
- `benchpress prover-show <prover>` to show the definition of a prover
- `benchpress list-files` to list the results
- `benchpress show <result>` to show the content of the result file
- `benchpress serve` to open a HTTP server on a port (default `8080`),
  which provides a basic web UI.

## Config File

Benchpress ships with a builtin config that contains, roughly:

### Builtin config

```sexp
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
  (unsat "unsat")
  (sat "^sat"))
```

The configuration is based on _stanzas_ that define available provers, available
sets of benchmarks (based on directories that contain  them), and _tasks_.
For now the only kind of supported task is to run provers on problems,
but it should get richer as we go (e.g. run proof checkers, do some basic CI,
run a task daily, etc.).

In this default file we also define a pseudo-prover, "smtlib-read-status",
which is used to parse SMTLIB benchmarks and find an annotation
`(set-info :status <…>)`. This is useful when running provers later
because it makes it easy to find bugs (if a prover reports a wrong answer).

We also define provers `minisat` and `z3` as common reference points,
providing info on how to run them (with `cmd …`) and how to parse their
results using regexes.

### Example of config file

A more complete example, taken from [mc2](https://github.com/c-cube/mc2):

```sexp

; from https://github.com/c-cube/mc2
(prover
  (name mc2)
  (cmd "ulimit -t $timeout; mc2 --time $timeout $file")
  (unsat "^Unsat")
  (sat "^Sat")
  (unknown "Unknown")
  (timeout "Timeout"))

(dir
  (path "$HOME/workspace/smtlib")
  (pattern ".*.smt2")
  (expect (run smtlib-read-status)))

(task
  (name glob-all-smtlib)
  (synopsis "run all SMT solvers on smtlib")
  (action
   (run_provers
    (dirs "$HOME/workspace/smtlib")
    (provers mc2 z3)
    ;(memory 100000000)  ; TODO: parse "10G"
    (timeout 10))))

(task
  (name glob-all-smtlib-QF_UF)
  (synopsis "run all SMT solvers on QF_UF")
  (action
    (run_provers
      (dirs "$HOME/workspace/smtlib/QF_UF")
      (provers mc2 z3)
      (timeout 10))))
```

Then one can run, say,
```sh
$ benchpress run -c the_file.sexp --task glob-all-smtlib-QF_UF -t 30
```
to run mc2 and z3 on the QF_UF problems in the SMTLIB directory.
The `task` stanza defines a pre-packaged task that can be launched easily
from the command line or the embedded web server (a bit like a makefile target).

### List of stanzas

The variable `$cur_dir` evaluates to the config file's directory. This allows
the config file to refer to provers that are installed locally (e.g. in the
same repository).

- `(prover …)` defines a new prover. The name should be unique.
  * `name`: unique name, used to refer to this prover in results, on the command line, etc
  * `cmd`: how to run the prover. Variables `$timeout`, `$file`, `$memory` are
    available and will refer to parameters used to run the prover on a file.
  * `sat`, `unsat`, `unknown`, `timeout`, `memory` are (perl) regex used to recognize
    the result (or reason for failure by timeout or memory exhaustion) of the prover.
- `(dir …)` defines a directory:
  * `(path …)` defines the path. The rules below apply to any file within this directory.
  * `(pattern ".*.smt2")` means only files matching the (perl) regex will be considered.
  * `(expect …)` defines how to find the expected result of each file (which will
    be compared to the actual result to detect bugs).
- `(task …)` defines a task that can be run from the command line.
  * `name` should be unique (used to refer to the task)
  * `action` defines what the task should do.
    For now there's only `(run_provers …)` to run provers on files locally.
- `(set-options…)` defines global options:
  * `j` integer for number of parallel tasks in `run`
  * `progress` boolean for progress bar in `run`

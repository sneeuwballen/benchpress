# Logitest

[![build status](https://travis-ci.org/nunchaku-inria/logitest.svg?branch=master "build status")](https://travis-ci.org/nunchaku-inria/logitest)

Tool to run one or more logic programs, on a set of files, and collect the
results.

License: BSD.

### Basic usage

```sh
$ logitest run -c foo.toml dir_a/ dir_b
…
```

### Options

- `logitest --help` to list options
- `logitest run`:
  * `-c <config file>` specify which TOML config file to use
  * `-t <time>` timeout (in seconds) for each run
  * `-m <memory>` memory limit in MB
  * `-F <file>` read list of problems from given file
  * `-p <prover1,prover2>` list of provers to use
  * `--profile <profile>` specify which test profile to use

### Config File

Example config file (in [toml](https://github.com/toml-lang/toml)):

```toml
provers = [ "nunchaku-cvc4", "nunchaku-paradox", "nunchaku-kodkod", "nunchaku-smbc" ]

[nunchaku-cvc4]

cmd = "nunchaku --checks -s cvc4 -nc --timeout $timeout $file 2>&1"
binary_deps = [ "cvc4" ]
unsat = "UNSAT"
sat = "SAT:"
unknown = "TIMEOUT|UNKNOWN"
timeout = "TIMEOUT"

[nunchaku-smbc]

cmd = "nunchaku --checks -s smbc -nc --timeout $timeout $file 2>&1"
binary_deps = [ "smbc" ]
unsat = "UNSAT"
sat = "SAT:"
unknown = "TIMEOUT|UNKNOWN"
timeout = "TIMEOUT"

[nunchaku-kodkod]

cmd = "nunchaku --checks -s kodkod -nc --timeout $timeout $file 2>&1"
binary_deps = [ "kodkodi" ]
unsat = "UNSAT"
sat = "SAT:"
unknown = "TIMEOUT|UNKNOWN"
timeout = "TIMEOUT"

[nunchaku-paradox]

cmd = "nunchaku --checks -s paradox -nc --timeout $timeout $file 2>&1"
binary_deps = [ "paradox" ]
unsat = "UNSAT"
sat = "SAT:"
unknown = "TIMEOUT|UNKNOWN"
timeout = "TIMEOUT"

[test]

memory = 4000
timeout=10
problems = ".*.nun"
provers = [ "nunchaku-cvc4", "nunchaku-paradox", "nunchaku-kodkod", "nunchaku-smbc" ]
dir = [ "fixed_bugs", "tests" ]
```

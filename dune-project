(lang dune 3.2)

(name benchpress)

(version 0.1)

(generate_opam_files true)

(source
 (github sneeuwballen/benchpress))

(authors "Simon Cruanes" "Guillaume Bury")

(maintainers "Simon Cruanes")

(license BSD-2-Clause)

(package
 (name benchpress)
 (synopsis "Tool to run one or more logic programs, on a set of files, and collect the results")
 (depends
  (ocaml
   (>= 4.14))
  dune
  base-unix
  (containers
   (and
    (>= 3.3)
    (< 4.0)))
  (eio (>= 1.0))
  (eio_posix (>= 1.0))
  (re
   (and
    (>= 1.8)
    (< 2.0)))
  csv
  (cmdliner
   (>= 1.1.0))
  ppx_subliner
  (iter
   (>= 1.0))
  logs
  base64
  ptime
  processor
  (pp_loc
   (and
    (>= 2.0)
    (< 3.0)))
  (sqlite3
   (>= 5.0.3))
  (sqlite3_utils
   (and
    (>= 0.4)
    (< 0.6)))
  (trace (>= 0.12))
  trace-tef
  (printbox
   (>= 0.6))
  (printbox-text
   (>= 0.6))
  (pbrt
   (>= 4.0))
  (pbrt_yojson
   (>= 4.0))
  camlzip
  (uuidm
   (>= 0.9.9))))

(package
 (name benchpress-lsp)
 (synopsis "LSP server for benchpress' config files")
 (depends
  dune
  (containers
   (and
    (>= 3.0)
    (< 4.0)))
  (eio (>= 1.0))
  (eio_posix (>= 1.0))
  (benchpress
   (= :version))
  (linol (>= 0.11))
  (linol-eio (>= 0.11))))

(package
 (name benchpress-server)
 (synopsis "Server and web UI for benchpress")
 (depends
  (ocaml
   (>= 5.1))
  dune
  base-unix
  (containers
   (and
    (>= 3.0)
    (< 4.0)))
  csv
  (benchpress
   (= :version))
  (cmdliner
   (>= 1.1.0))
  logs
  uuidm
  base64
  sqlite3
  (sqlite3_utils
   (and
    (>= 0.4)
    (< 0.5)))
  (tiny_httpd (>= 0.17))
  tiny_httpd_eio
  (eio (>= 1.0))
  (eio_posix (>= 1.0))
  (printbox
   (>= 0.12))
  (printbox-text
   (>= 0.12))
  (jemalloc
   (and
    (>= 0.2)
    (< 0.3)))
  (mirage-crypto-rng
   (>= 2.0))
  (twirp_tiny_httpd
   (>= 0.2))
  (opentelemetry (>= 0.90))
  (opentelemetry-client-ocurl (>= 0.90))
  (pbrt
   (>= 4.0))
  (pbrt_services
   (>= 4.0))
  (pbrt_yojson
   (>= 4.0))
  (benchpress-proto
   (= :version))))

(package
 (name benchpress-worker)
 (synopsis "A benchpress helper worker")
 (depends
  (ocaml
   (>= 5.1))
  dune
  (containers
   (and
    (>= 3.0)
    (< 4.0)))
  (benchpress
   (= :version))
  (cmdliner
   (>= 1.1.0))))

(package
 (name benchpress-proto)
 (synopsis "Protobuf/Twirp API types for benchpress")
 (depends
  dune
  (ocaml
   (>= 5.1))
  (pbrt
   (>= 4.0))
  (pbrt_services
   (>= 4.0))
  (pbrt_yojson
   (>= 4.0))
  yojson))

(package
 (name benchpressctl)
 (synopsis "Client tool for benchpress-server")
 (depends
  dune
  (ocaml
   (>= 5.1))
  (cmdliner
   (>= 1.1.0))
  curly
  (twirp_core
   (>= 0.2))
  ppx_subliner
  (pbrt
   (>= 4.0))
  (pbrt_services
   (>= 4.0))
  (pbrt_yojson
   (>= 4.0))
  yojson
  (benchpress-proto
   (= :version))))

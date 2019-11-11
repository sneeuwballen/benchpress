# build image
FROM ocaml/opam2:alpine-3.8-ocaml-4.06 as build
WORKDIR /logitest/
RUN opam update -y && opam upg -y
COPY --chown=opam:nogroup src *.opam dune-project ./
RUN sudo apk update && sudo apk add m4
RUN eval `opam env` && \
    opam pin . -y -n && \
    opam install logitest && \
    sudo cp `which logitest` ./

# production image
FROM alpine:latest AS prod
WORKDIR /logitest
COPY --from=build /logitest/logitest .
RUN [ "./logitest" ]

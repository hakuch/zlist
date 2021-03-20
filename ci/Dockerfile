FROM ocaml/opam-dev:ubuntu
RUN opam remote remove default --all
RUN opam remote add default https://opam.ocaml.org/2.0
RUN opam install dune odoc ppx_expect

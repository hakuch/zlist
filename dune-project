(lang dune 2.7)
(name zlist)
(generate_opam_files true)

(authors "Jesse Haber-Kucharsky <jesse@haber-kucharsky.com")
(maintainers "Jesse Haber-Kucharsky <jesse@haber-kucharsky.com")
(homepage "https://gitlab.com/jhaberku/Zlist")
(source (uri "git+https://gitlab.com/jhaberku/Zlist"))
(bug_reports "https://gitlab.com/jhaberku/Zlist/issues")
(license Apache-2.0)

(package
 (name zlist)
 (synopsis "Lazy lists for OCaml")
 (description "zlist consists of the definition of a lazy list type and a number of useful functions for manipulating and constructing lazy lists.")
 (depends
  (ppx_expect :with-test)
  (ppx_inline_test :with-test)))
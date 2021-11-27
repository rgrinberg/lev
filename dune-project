(lang dune 2.9)

(name lev)

(generate_opam_files true)

(license ISC)
(maintainers "Rudi Grinberg <me@rgrinberg.com>")
(authors "Rudi Grinberg <me@rgrinberg.com>")
(source (github rgrinberg/lev))

(implicit_transitive_deps false)

(package
 (synopsis "bindings to libev")
 (description "low level bindings to libev")
 (name lev)
 (depends
  (ppx_expect :with-test)
  base-unix))

(package
 (synopsis "Fiber + Lev")
 (description "High level API based on dune's fibers")
 (name lev-fiber)
 (depends
  (ppx_expect :with-test)
  fiber
  stdune
  base-unix))

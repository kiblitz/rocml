(lang dune 3.17)
(name rocml)
(generate_opam_files true)

(source (github kiblitz/rocml))
(authors "glee")
(maintainers "glee")
(license MIT)
(documentation https://github.com/kiblitz/rocml)

(package
 (name rocml)
 (synopsis "rocm ocaml")
 (description "pronounced 'roh-ka-mohl'")
 (depends ocaml)
 (tags ("rocm")))

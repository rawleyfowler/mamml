(lang dune 3.4)

(name mamml)
(generate_opam_files true)

(package
 (name mamml)
 (allow_empty)
 (synopsis "A json caching database for OCaml")
 (description "^")
 (depends
   ocaml
   dune
   yojson)
 (tags
  (topics database)))
  
(source
 (github rawleyfowler/mamml))

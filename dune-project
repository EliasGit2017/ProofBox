(lang dune 2.9.3)
(name proofbox)

(source (uri git://github.com/EliasGit2017/ProofBox))
(license MIT)
(authors "Elias Bendjaballah")
(maintainers "elias.bendjaballah@gmail.com")
(homepage https://github.com/EliasGit2017/ProofBox)
(bug_reports https://github.com/EliasGit2017/ProofBox/issues)

(package
 (name proofbox)
 (synopsis "ProofBox API REST")
 (depends
  vue-jsoo
  ez_api
  calendar
  geoip
  cohttp-lwt-unix
  re
  pgocaml_ppx
  ez_pgocaml
  (ppx_deriving_encoding ( >= 0.3.0 ))
  ))

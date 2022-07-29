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
  (vue-jsoo (= 0.2))
  (ez_api (= 1.2.0))
  (calendar (= 2.04))
  (geoip (= 0.0.3))
  (cohttp-lwt-unix (= 5.0.0))
  (re (= 1.10.4))
  (pgocaml_ppx (= 4.3.0+ocp))
  (ez_pgocaml (= 1.0.0))
  (ppx_deriving_encoding ( >= 0.3.0 ))
  ))

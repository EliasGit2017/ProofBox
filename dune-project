(lang dune 2.9.3)
(name proofbox)

(source (uri git://gitlab.com/o-labs/skeleton))
(license MIT)
(authors "Elias Bendjaballah")
(maintainers "elias.bendjaballah@gmail.com")
(homepage )
(bug_reports )

(package
 (name skeleton)
 (synopsis "Skeleton of website with API REST")
 (depends
  vue-jsoo
  ez_api
  calendar
  geoip
  cohttp-lwt-unix
  re
  pgocaml_ppx
  ez_pgocaml))

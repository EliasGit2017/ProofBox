open Data_types
open Encoding
open EzAPI

let section_main = Doc.section "Main services"
let sections = [ section_main ]

let version : (version, exn, no_security) service0 =
  service
    ~section:section_main
    ~name:"version"
    ~output:version_enc
    Path.(root // "version")



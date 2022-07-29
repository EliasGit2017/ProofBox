open Data_types
open Encoding
open EzAPI
open Utils

let section_main = Doc.section "Main services"
let sections = [ section_main ]


let version_test =
  Arg.make
    ~name:"test json body"
    ~destruct:(to_result ~convf:version_test_of_string)
    ~construct:version_test_to_string
    ()


module Errors = struct
  let server_errors = [
    (* Generic error, for testing purposes *)
    Err.make
    ~code:500
    ~name:"Base error"
    ~encoding:Json_encoding.unit
    ~select:(function Invalid_request -> Some () | _ -> None)
    ~deselect:(fun () -> Unknown)
  ]
end


let version_test_json_body : (request_v, version, server_error_type, no_security) post_service0 =
  post_service
    ~section:section_main
    ~name:"version"
    ~descr:"Trying to add json in body"
    (* ~meth:`GET *)
    ~params:[]
    ~input:request_v_enc
    ~output:version_enc
    ~errors:Errors.server_errors
    Path.(root // "version_json_body")

let version : (version, server_error_type, no_security) service0 =
  service
    ~section:section_main
    ~name:"version"
    ~descr:"template/skeleton service"
    ~meth:`GET
    ~params:[]
    ~output:version_enc
    ~errors:Errors.server_errors
    Path.(root // "version")




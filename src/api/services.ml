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

let sr_job_desc : (job_desc_req, jobs, server_error_type, no_security) post_service0 =
  post_service
  ~section:section_main
  ~name:"retrieve_job_desc"
  ~descr:"get job description in db"
  (* ~meth:`GET *)
  ~params:[]
  ~input:Encoding.job_desc_req_enc
  ~output:Encoding.jobs
  ~errors:Errors.server_errors
  Path.(root // "retrieve_job_description")


let sr_job_desc_from_user : (all_jobs_get, jobs, server_error_type, no_security) post_service0 =
  post_service
  ~section:section_main
  ~name:"retrieve_job_desc"
  ~descr:"get job description in db"
  (* ~meth:`GET *)
  ~params:[]
  ~input:Encoding.all_jobs_get_enc
  ~output:Encoding.jobs
  ~errors:Errors.server_errors
  Path.(root // "retrieve_job_description_from_user")

open Data_types
open Encoding
open EzAPI
open Utils

let section_main = Doc.section "Main services"
let sections = [ section_main ]

let arg_test =
  Arg.string ~example:"example-of-value" "arg-in-path"

let param_arg =
    Param.string ~name:"arg-string" ~descr:"An example argument" "arg"

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
    ~params:[]
    ~input:Encoding.job_desc_req_enc
    ~output:Encoding.jobs
    ~errors:Errors.server_errors
    Path.(root // "retrieve_job_description")


let sr_job_desc_from_user : (all_jobs_get, jobs, server_error_type, no_security) post_service0 =
  post_service
    ~section:section_main
    ~name:"retrieve_job_desc"
    ~descr:"get all jobs description in db (table jobs_description) from user"
    ~params:[]
    ~input:all_jobs_get_enc
    ~output:jobs
    ~errors:Errors.server_errors
    Path.(root // "retrieve_job_description_from_user")

let test_session : (string, request_v, version, server_error_type, no_security) post_service1 =
  post_service
    ~section:section_main
    ~name:"version"
    ~descr:"Trying EzApi Session"
    ~params:[param_arg]
    ~input:request_v_enc
    ~output:version_enc
    ~errors:Errors.server_errors
    Path.(root // "version_session" /: arg_test)


let sign_up_new_user : (user_description, general_comm, server_error_type, no_security) post_service0 =
  post_service
    ~section:section_main
    ~name:"Sign Up"
    ~descr:"Trying EzApi Session and sign up"
    ~params:[]
    ~input:user_description_enc
    ~output:general_comm_enc
    ~errors:Errors.server_errors
    Path.(root // "signup_return_auth_info")

(** Service to transfer zip archive from client to server *)
let zip_tranfer : (string, string (* general_comm *), server_error_type, no_security) ws_service0 =
  ws_service
  ~section:section_main
  ~name:"tranfer zip"
  ~descr:"Sending ZIP archive"
  ~params:[]
  ~input:(Raw mime_zip)
  ~output:(Json Json_encoding.string)
  ~errors:Errors.server_errors
  Path.(root // "zip_send")

(** Service to transfer zip archive from client to server *)
let post_zip_send : (general_comm2, general_comm2 , server_error_type, no_security) post_service0 =
  post_service
  ~section:section_main
  ~name:"post : tranfer zip"
  ~descr:"Sending ZIP archive"
  ~params:[]
  ~input:general_comm2_enc
  ~output:general_comm2_enc
  ~errors:Errors.server_errors
  Path.(root // "base_blob_ws")

let send_job_metadata : (meta_payload, jobs, server_error_type, no_security) post_service0 =
  post_service
  ~section:section_main
  ~name:"send_job_metadata"
  ~descr:"send metadata associated to a job (and ask for server availability)"
  ~params:[]
  ~input:meta_payload_enc
  ~output:jobs
  ~errors:Errors.server_errors
  Path.(root // "job_metadata")

(* let send_job_metadata_and_payload : (meta_payload, meta_payload, server_error_type, no_security) ws_service0 =
  ws_service
  ~section:section_main
  ~name:"send_job_metadata"
  ~descr:"send metadata associated to a job (and ask for server availability)"
  ~params:[]
  ~input:(Json meta_payload_enc)
  ~output:(Json meta_payload_enc)
  ~errors:Errors.server_errors
  Path.(root // "job_metadata_and_payload") *)

let send_job_main_service : (job_payload, job_payload, server_error_type, no_security) post_service0 =
  post_service
  ~section:section_main
  ~name:"Job submission through post0 service containing blob zip archive in "
  ~descr:"Sending ZIP archive & associated metadata"
  ~params:[]
  ~input:job_payload_enc
  ~output:job_payload_enc
  ~errors:Errors.server_errors
  Path.(root // "main_job_send")

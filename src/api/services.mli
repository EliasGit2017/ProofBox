val section_main : EzAPI.Doc.section
val sections : EzAPI.Doc.section list
val arg_test : string EzAPI.Arg.t
val param_arg : EzAPI.Param.t
val version_test : Data_types.version EzAPI.Arg.t
module Errors :
  sig
    val server_errors : Data_types.server_error_type EzAPI.Err.case list
  end
val version :
  (Encoding.version, Data_types.server_error_type, EzAPI.no_security)
  EzAPI.service0
val version_test_json_body :
  (Encoding.request_v, Encoding.version, Data_types.server_error_type,
   EzAPI.no_security)
  EzAPI.post_service0
val sr_job_desc :
  (Encoding.job_desc_req, Data_types.jobs, Data_types.server_error_type,
   EzAPI.no_security)
  EzAPI.post_service0
val sr_job_desc_from_user :
  (Encoding.all_jobs_get, Data_types.jobs, Data_types.server_error_type,
   EzAPI.no_security)
  EzAPI.post_service0
val test_session :
  (string, Encoding.request_v, Encoding.version,
   Data_types.server_error_type, EzAPI.no_security)
  EzAPI.post_service1
val sign_up_new_user :
  (Encoding.user_description, Encoding.general_comm,
   Data_types.server_error_type, EzAPI.no_security)
  EzAPI.post_service0
val zip_tranfer :
  (string, string, Data_types.server_error_type, EzAPI.no_security)
  EzAPI.ws_service0
val post_zip_send :
  (Encoding.general_comm2, Encoding.general_comm2,
   Data_types.server_error_type, EzAPI.no_security)
  EzAPI.post_service0
val send_job_metadata :
  (Encoding.meta_payload, Data_types.jobs, Data_types.server_error_type,
   EzAPI.no_security)
  EzAPI.post_service0
val send_job_main_service :
  (Encoding.job_payload, Encoding.job_payload, Data_types.server_error_type,
   EzAPI.no_security)
  EzAPI.post_service0

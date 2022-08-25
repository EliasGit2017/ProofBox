open Json_encoding
open Data_types


type nonrec version = Data_types.version = {
  v_db : string;
  v_db_version : int;
} [@@deriving json_encoding {remove_prefix = false}]

type nonrec request_v = Data_types.request_v = {
  basic: string;
}[@@deriving json_encoding]

type nonrec job_desc_req = Data_types.job_desc_req = {
  job_client : string;
  job_ref_tag_v : int;
}[@@deriving json_encoding {remove_prefix = false}]

type nonrec all_jobs_get = Data_types.all_jobs_get = {
  job_client_req : string;
}[@@deriving json_encoding]

type nonrec jobs_descr = Data_types.jobs_descr = {
  job_client : string;
  job_ref_tag : int;
  order_ts : string;
  path_to_f : string;
  priority : int;
  status : string;
}[@@deriving json_encoding]

let jobs = list jobs_descr_enc


let api_config = obj1 (opt "port" int)

let info_encoding = conv
(fun {www_apis} -> www_apis)
(fun www_apis -> {www_apis}) @@
obj1
(req "apis" (list string))

(*****************************************************************************)

let user_info = string

type nonrec user_description = Data_types.user_description = {
  username : string;
  email : string;
  password : string;
  user_desc : string;
  first_login_date : string;
} [@@deriving json_encoding]

let all_users = list user_description_enc

type nonrec general_comm = Data_types.general_comm = {
  comm_desc : string;
  client_infos : string;
  infos : string;
  error_desc : string;
} [@@deriving json_encoding]

type nonrec meta_payload = Data_types.meta_payload = {
  archive_name : string;
  client_id : string;
  comment : string;
  priority : int;
  checksum_type : string;
  checksum : string;
  info : string;
  error : string;
  code : int;
} [@@deriving json_encoding]


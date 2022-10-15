open Json_encoding
open Data_types

type nonrec version = Data_types.version = { v_db : string; v_db_version : int }
[@@deriving json_encoding { remove_prefix = false }]

type nonrec request_v = Data_types.request_v = { basic : string }
[@@deriving json_encoding]

type nonrec job_desc_req = Data_types.job_desc_req = {
  job_client : string;
  job_ref_tag_v : int;
}
[@@deriving json_encoding { remove_prefix = false }]

type nonrec all_jobs_get = Data_types.all_jobs_get = { job_client_req : string }
[@@deriving json_encoding]

type nonrec jobs_descr = Data_types.jobs_descr = {
  job_client : string;
  job_ref_tag : int;
  order_ts : string;
  path_to_f : string;
  checksum_type : string;
  checksum : string;
  priority : int;
  status : string;
}
[@@deriving json_encoding]

let jobs = list jobs_descr_enc
let api_config = obj1 (opt "port" int)

let info_encoding =
  conv (fun { www_apis } -> www_apis) (fun www_apis -> { www_apis })
  @@ obj1 (req "apis" (list string))

(*****************************************************************************)

let user_info = string

type nonrec user_description = Data_types.user_description = {
  username : string;
  email : string;
  password : string;
  user_desc : string;
  first_login_date : string;
}
[@@deriving json_encoding]

let all_users = list user_description_enc

type nonrec general_comm = Data_types.general_comm = {
  comm_desc : string;
  client_infos : string;
  infos : string;
  error_desc : string;
}
[@@deriving json_encoding]

type nonrec general_comm2 = Data_types.general_comm2 = {
  comm_desc_2 : string;
  client_infos : string;
  infos_b : int list;
  checksum_type : string;
  checksum : string;
  error_desc : string;
}
[@@deriving json_encoding { remove_prefix = false }]

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
}
[@@deriving json_encoding]

type nonrec job_payload = Data_types.job_payload = {
  job_archive_name : string; (* zip results *)
  job_client_id : string; (* client *)
  desc : string; (* job_id *)
  infos_pb : int list; (* zip *)
  checksum_type : string; (* md5 *)
  checksum : string; (* md5 *)
  priority : int; (* -100 *)
  job_return : jobs_descr list; (* jobs in cache for client x *)
  code : int; (* 200 *)
}
[@@deriving json_encoding { remove_prefix = false }]

type nonrec job_cache = Data_types.job_cache = {
  job_id : int;
  path_to_res : string;
  time : string;
  status : string;
}[@@deriving json_encoding { remove_prefix = false }]

type nonrec job_payload_cache = Data_types.job_payload_cache = {
  job_archive_name : string;
  job_client_id : string;
  desc : string;
  job_return : job_cache list;
  code : int;
}[@@deriving json_encoding { remove_prefix = false }]
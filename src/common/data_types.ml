open EzAPI
(** Module [Data_type] defines all the types (input, queries, server errors ...) used by server and client *)

type www_server_info = { www_apis : string list }
(** Type that lists all server host names *)

type user_info = string
type version = { v_db : string; v_db_version : int }
type request_v = { basic : string }

(* **************************************************** *)

(* Job types and handling *)

type job_desc_req = { job_client : string; job_ref_tag_v : int }
type all_jobs_get = { job_client_req : string }

type jobs_descr = {
  job_client : string;
  job_ref_tag : int;
  order_ts : string;
  path_to_f : string;
  checksum_type : string;
  checksum : string;
  priority : int;
  status : string;
}

type nonrec jobs = jobs_descr list

(* **************************************************** *)

(* Error types *)

type server_error_type = Invalid_request | No_sources_config | Unknown

exception Proofbox_api_error of server_error_type

let proofbox_api_error typ = Proofbox_api_error typ

(** Decapsulate server_error_type *)
let server_error_type err =
  match err with Proofbox_api_error typ -> typ | _ -> Unknown

(* **************************************************** *)

type user_description = {
  username : string;
  email : string;
  password : string;
  user_desc : string;
  first_login_date : string;
}

type nonrec all_users = user_description list

type general_comm = {
  comm_desc : string;
  client_infos : string;
  infos : string;
  error_desc : string;
}

type general_comm2 = {
  comm_desc_2 : string;
  client_infos : string;
  infos_b : int list;
  checksum_type : string;
  checksum : string;
  error_desc : string;
}

type data_transfer = EzAPI.Mime.t list

type meta_payload = {
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

type job_payload = {
  job_archive_name : string;
  job_client_id : string;
  desc : string;
  infos_pb : int list;
  checksum_type : string;
  checksum : string;
  priority : int;
  job_return : jobs_descr list;
  code : int;
}

type job_cache = {
  job_id : int;
  path_to_res : string;
  time : string;
  status : string;
}

type job_payload_cache = {
  job_archive_name : string;
  job_client_id : string;
  desc : string;
  job_return : job_cache list;
  code : int;
}

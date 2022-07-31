(** Module [Data_type] defines all the types (input, queries, server errors ...) used by server and client *)

type www_server_info = {
  www_apis : string list;
}
(** Type that lists all server host names *)

type version = {
  v_db: string;
  v_db_version: int;
}

type request_v = {
  basic: string;
}

type job_desc_req = {
  job_client : string;
  job_ref_tag_v : string;
}

type jobs_descr = {
  job_client : string;
  job_ref_tag : string;
  order_ts : string;
  path_to_f : string;
  status : string;
}

type nonrec jobs = jobs_descr list

(* type main_jobs =
  | Jobs of jobs *)

type server_error_type =
  | Invalid_request
  | No_sources_config
  | Unknown

exception Proofbox_api_error of server_error_type

let proofbox_api_error typ = Proofbox_api_error typ

let server_error_type err =
  match err with
  | Proofbox_api_error typ -> typ
  | _ -> Unknown
(** Decapsulate server_error_type *)

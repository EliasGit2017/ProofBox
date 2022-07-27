type version = {
  v_db: string;
  v_db_version: int;
}

type request_v = {
  basic: string;
}

type www_server_info = {
  www_apis : string list;
}

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
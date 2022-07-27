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

exception Search_api_error of server_error_type



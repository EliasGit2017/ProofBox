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
(* Unused data structures, variables ... TO CLEAN*)

(* let main_jobs =
  let cases =
    [case
    ~title:"Jobs"
    jobs
    (function | Jobs s -> Some s )
    (function s -> Jobs s);
    ]
  in union cases *)

(* let version = conv
  (fun {v_db; v_db_version} -> (v_db, v_db_version))
  (fun (v_db, v_db_version) -> {v_db; v_db_version}) @@
  obj2
    (req "db" string)
    (req "db_version" int) *)

let user_info = string
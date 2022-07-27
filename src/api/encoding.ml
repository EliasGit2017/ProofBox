open Json_encoding
open Data_types

(* let version = conv
  (fun {v_db; v_db_version} -> (v_db, v_db_version))
  (fun (v_db, v_db_version) -> {v_db; v_db_version}) @@
  obj2
    (req "db" string)
    (req "db_version" int) *)

type nonrec version = Data_types.version = {
  v_db : string;
  v_db_version : int;
} [@@deriving json_encoding {remove_prefix = false}]

type nonrec request_v = Data_types.request_v = {
  basic: string;
}[@@deriving json_encoding]

let api_config = obj1 (opt "port" int)

let info_encoding = conv
    (fun {www_apis} -> www_apis)
    (fun www_apis -> {www_apis}) @@
  obj1
    (req "apis" (list string))


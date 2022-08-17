open Otoml

(* Utils to get / set values and acces toml files simply &
   Wrappers over some functions *)

let stringl_to_str s_l = 
  List.fold_left ( ^ ) "" s_l


let get_value_wrap parsed_toml path_toval =
  try Otoml.find parsed_toml path_toval with
  | Otoml.Type_error e -> 
  | Otoml.Key_error e -> 

let get_owner_username = ()
let get_owner_bio = ()
let get_owner_bio = ()
let get_owner_password = ()
let get_job_description_job_id = ()
let get_job_description_solver = ()
let get_job_description_solver_version = ()
let get_job_description_job_synopsis = ()
let get_job_description_path_tof = ()

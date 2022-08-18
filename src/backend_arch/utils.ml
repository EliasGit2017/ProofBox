open Otoml

(* Utils to get / set values and acces toml files simply &
   Wrappers over some functions *)

let owner_username = [ "owner"; "username" ]
let owner_email = [ "owner"; "email" ]
let owner_bio = [ "owner"; "bio" ]
let owner_password = [ "owner"; "password" ]
let job_description_id = [ "job_description"; "job_id" ]
let job_description_solver = [ "job_description"; "solver" ]
let job_description_solver_version = [ "job_description"; "solver_version" ]
let job_description_synopsis = [ "job_description"; "synopsis" ]
let job_description_path_tof = [ "job_description"; "path_to_client_repo" ]

(** Bad list to str *)
let stringl_to_str s_l = List.fold_left ( ^ ) "" s_l

(** change Otoml.TomlBoolean to bool (decaps/encaps) *)
let get_value_wrap parsed_toml path_toval =
  try Otoml.find parsed_toml Otoml.get_value path_toval with
  | Type_error e ->
      print_endline
      @@ Printf.sprintf "Otoml.Type_error from get_value_wrap : %s" e;
      Otoml.TomlBoolean false
  | Key_error e ->
      print_endline
      @@ Printf.sprintf "Otoml.Type_error from get_value_wrap : %s" e;
      Otoml.TomlBoolean false

let get_owner_username parsed_toml = get_value_wrap parsed_toml owner_username
let get_owner_bio parsed_toml = get_value_wrap parsed_toml owner_bio
let get_owner_bio parsed_toml = get_value_wrap parsed_toml owner_bio
let get_owner_password parsed_toml = get_value_wrap parsed_toml owner_password

let get_job_description_job_id parsed_toml =
  get_value_wrap parsed_toml job_description_id

let get_job_description_solver parsed_toml =
  get_value_wrap parsed_toml job_description_solver

let get_job_description_solver_version parsed_toml =
  get_value_wrap parsed_toml job_description_solver_version

let get_job_description_job_synopsis parsed_toml =
  get_value_wrap parsed_toml job_description_synopsis

let get_job_description_path_tof parsed_toml =
  get_value_wrap parsed_toml job_description_path_tof


(* Unix tools *)

let stat_code status =
  match status with
  | Unix.WEXITED e -> Printf.sprintf "WEXITED : code = %s" (string_of_int e)
  | Unix.WSIGNALED s -> Printf.sprintf "WSIGNALED : code = %s" (string_of_int s)
  | Unix.WSTOPPED st -> Printf.sprintf "WSTOPPED : code = %s" (string_of_int st)

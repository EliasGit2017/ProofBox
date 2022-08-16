open Data_types
open Str

(* Conversion & data printing : ( *_to_string, *_of_string, etc) *)

(** [to_result str ~convf] encapsulates application of [convf] on [str] within [result] type *)
let to_result :
    type conv. string -> convf:(string -> conv) -> (conv, string) result =
 fun str ~convf ->
  try Ok (convf str)
  with Failure str -> Error ("Not recognized data_type : " ^ str)

(* ****** Convert / Create some records to / from string(s) for printing/debug *********** *)

(** Create [Data_types.version] from string where record field are separated by '+'
    (to generalize) *)
let version_test_of_string str =
  match String.split_on_char '+' str with
  | [ v_db; v_db_version ] ->
      let v_db_version = int_of_string v_db_version in
      { v_db; v_db_version }
  | _ -> failwith ("Not valid entry info : " ^ str)

(** Create string describing [Data_types.version] with '+' as separator
    (to genralize) *)
let version_test_to_string { v_db; v_db_version } =
  Printf.sprintf "%s+%d" v_db v_db_version

(** Create string describing [Data_types.user_description] .
    Here for debugging/verbose purposes only. *)
let users_to_string { username; email; password; user_desc; first_login_date } =
  Printf.sprintf
    "username = %s\n\
     email = %s\n\
     password = %s\n\
     user_description = %s\n\
     first_login_date = %s\n"
    username email password user_desc first_login_date

let default_server_response_from_string _comm_des _client_infos _infos
    error_desc =
  {
    comm_desc = _comm_des;
    client_infos = _client_infos;
    infos = _infos;
    error_desc;
  }

let default_server_response_to_string elem =
  Printf.sprintf
    "comm_desc = %s; client_infos = %s; infos = %s; error_desc = %s"
    elem.comm_desc elem.client_infos elem.infos elem.error_desc

(*****************************************************************************)

(* Utilities : regex, data manipulation & transformation *)

(** Regex check on email : pattern identical to domain attempt in
    [db/versions.ml] *)
let check_email_validity email =
  let right_email = Str.regexp "\\([^<>(),; \t]+@[^<>(),; \t]+\\)$" in
  Str.string_match right_email email 0

(** Regex check on password rules :
    At least one digit [0-9]
    At least one lowercase character [a-z]
    At least one uppercase character [A-Z]
    ** At least one special character [\[*.!@#$%^&(){}[]:;<>,.?/~_+-=|\]]
    At least 8 characters in length, but no more than 32.*)
let check_password_validity password =
  let right_password =
    Str.regexp {|^\(.{0,7}\|[^0-9]*\|[^A-Z]*\|[^a-z]*\|[a-zA-Z0-9]*\)$|}
  in
  not @@ Str.string_match right_password password 0
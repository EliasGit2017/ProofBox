open Data_types


(* Conversion & data printing *)

(** [to_result str ~convf] encapsulates application of [convf] on [str] within [result] type *)
let to_result :
    type conv. string -> convf:(string -> conv) -> (conv, string) result =
 fun str ~convf ->
  try Ok (convf str)
  with Failure str -> Error ("Not recognized data_type : " ^ str)

(* ********* Might not be so usefull *********** *)

(**  *)
let version_test_of_string str =
  match String.split_on_char '+' str with
  | [ v_db; v_db_version ] ->
      let v_db_version = int_of_string v_db_version in
      { v_db; v_db_version }
  | _ -> failwith ("Not valid entry info : " ^ str)

(**  *)
let version_test_to_string { v_db; v_db_version } =
  Printf.sprintf "%s+%d" v_db v_db_version

let users_to_string { username; email; password; user_desc; first_login_date } =
  Printf.sprintf
    "username = %s\n\
     email = %s\n\
     password = %s\n\
     user_description = %s\n\
     first_login_date = %s\n"
    username email password user_desc first_login_date

(* Utilities *)

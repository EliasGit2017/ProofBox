open Data_types
open Str
open EzAPI

(* Conversion & data printing : ( *_to_string, *_of_string, etc) *)

let rec stringlist_tostring sep = function
  | [] -> ""
  | "" :: l -> stringlist_tostring sep l
  | e :: l -> e ^ sep ^ stringlist_tostring sep l

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

let job_list_to_string job_l =
  List.fold_left
    (fun res
         {
           job_client;
           job_ref_tag;
           order_ts;
           path_to_f;
           checksum_type;
           checksum;
           priority;
           status;
         } ->
      res
      ^ Printf.sprintf
          "{ job_client = %s; job_ref_tag = %d; order_ts = %s; path_to_f =%s ; \
           checksum_type = %s; checksum = %s; priority = %d; status = %s }\n"
          job_client job_ref_tag order_ts path_to_f checksum_type checksum
          priority status)
    "" job_l

(** Default server response from args : TO REDEFINE *)
let default_server_response_from_string _comm_des _client_infos _infos
    error_desc =
  {
    comm_desc = _comm_des;
    client_infos = _client_infos;
    infos = _infos;
    error_desc;
  }

(** Default server response to string : TO REDEFINE *)
let default_server_response_to_string elem =
  Printf.sprintf
    "{\ncomm_desc = %s;\nclient_infos = %s;\ninfos = %s;\nerror_desc = %s\n}"
    elem.comm_desc elem.client_infos elem.infos elem.error_desc

(** [Data_types.meta_payload] to string *)
let meta_payload_to_string (meta : Data_types.meta_payload) =
  Printf.sprintf
    "archive_name = %s; client_id = %s; comment = %s; priority = %d; \
     checksum_type = %s; checksum = %s; info = %s; error = %s; code = %d"
    meta.archive_name meta.client_id meta.comment meta.priority
    meta.checksum_type meta.checksum meta.info meta.error meta.code

(** string args to [Data_types.meta_payload] *)
let meta_payload_from_string archive_name client_id comment priority
    checksum_type checksum info error code =
  {
    archive_name;
    client_id;
    comment;
    priority;
    checksum_type;
    checksum;
    info;
    error;
    code;
  }
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

(** Returns a random uuid like string which is the directory name in which
    the server will store client zip + results *)
let rand_uuid_gen () =
  Random.init (Random.int 29000);
  (* this is bad ... really bad -> replaced with root + timestamp + client-name  *)
  (Uuidm.v4_gen (Random.get_state ()) () |> Uuidm.to_string) ^ "/"

(** Generates parent directory ... choose between parent_d_name & rand_uuid_gen *)
let parent_d_name root_fname client_name =
  root_fname ^ string_of_float (Unix.gettimeofday ()) ^ "-" ^ client_name ^ "/"

(*****************************************************************************)

(** Retrieve bytes from source file [fn] and send it to server (used by client only) *)
let get_bytes fn =
  let inc = open_in_bin fn in
  let rec lect acc =
    match input_char inc with
    | b -> lect (Char.code b :: acc)
    | exception End_of_file -> List.rev acc
  in
  let res = lect [] in
  close_in inc;
  res

(** Write the bytes read by [get_bytes] to file destination *)
let write_to_dest fn ints =
  let outc = open_out_bin fn in
  List.iter (fun b -> output_char outc (Char.chr b)) ints;
  close_out outc

(* websocket handling *)
let mime_zip = [ Option.get @@ Mime.parse "application/zip" ]
let oct_stream = [ Option.get @@ Mime.parse "application/octet-stream" ]

(** Receive zip from websocket and place it in the adequate directory *)
let retrieve_zip_from_string dest_filename contents =
  let w_to_f = open_out dest_filename in
  Printf.fprintf w_to_f "%s\n" contents;
  close_out w_to_f

let retrieve_zip_from_bytes dest_filename contents =
  let w_to_f = open_out_bin dest_filename in
  output_bytes w_to_f contents;
  close_out w_to_f

(** Returns the human readable MD5 string associated to [file_name] *)
let md5_checksum file_name = Digest.file file_name |> Digest.to_hex

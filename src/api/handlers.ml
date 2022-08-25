open Lwt.Infix
open Data_types
open Db
open Bcrypt
open Utils

(* ****************************************************************** *)

let root_files = "/home/elias/OCP/ez_proofbox/scripts/Containers/storage/"

(* Redefine this in its own file when mastered *)

module SessionArg = struct
  type user_id = string
  type user_info = Data_types.user_info

  let user_id_encoding = Json_encoding.string
  let user_info_encoding = Encoding.user_info
  let rpc_path = []
  let token_kind = `CSRF "X-Csrf-Token" (* `Cookie "EZSESSION" *)
end

module Registered_Users = EzSessionServer.UserStoreInMemory (SessionArg)
module My_Session = EzSessionServer.Make (Registered_Users)

(* ****************************************************************** *)

(** Initial to_api (return bind promise [p] with Lwt) *)
let to_api_v0 p = Lwt.bind p EzAPIServerUtils.return

(** [to_api p] executes [p] asynchroniously and encapsulates promise value to answer [EzAPIServerUtils.Answer.t].
    Catches and encapsulates server errors raised by [p] in order to get error's constructors
    [Data_types.server_error_type] from client-side. If [p] raises another type of error, then it is
    converted to [Unknown].*)
let to_api p =
  Lwt.catch
    (fun () -> Lwt.bind p EzAPIServerUtils.return)
    (fun err ->
      match err with
      | Proofbox_api_error exn -> EzAPIServerUtils.return ~code:500 (Error exn)
      | _ -> EzAPIServerUtils.return ~code:500 (Error Unknown))

(* ****************************************************************** *)

(** Hash function using the Bcrypt2 algorithm from the
    [safepass] package : 
    see https://github.com/darioteixeira/ocaml-safepass
    for documentation *)
let hash_bcrypt pre_hashed_password =
  Bcrypt.string_of_hash @@ Bcrypt.hash ~count:8 pre_hashed_password

let hash_user_desc ud = { ud with password = hash_bcrypt ud.password }

(** [Registered_Users.create_user] wrapper for server side session management 
    (not used consistently)*)
let register_user user_d =
  Registered_Users.create_user ~login:user_d.username
    ~password:(hash_bcrypt user_d.password)
    user_d.user_desc

(** Load sessions from predefined users (dummy values) 
    in Database [users] *)
let load_predefined_users =
  Db.get_all_users () >|= fun users ->
  let res = List.map Utils.users_to_string users in
  let _ = List.map print_endline res in
  List.map
    (fun e ->
      Registered_Users.create_user ~login:e.username ~password:e.password
        e.user_desc)
    users

(* ****************************************************************** *)

(** Dummy [result Lwt.t] return value (used for debug in some handlers/
    services) : (not encapsulated in [result EzAPIServerUtils.Answer.t Lwt.t]) *)
let dummy_response () =
  Db.get_version () >|= fun v_db_version ->
  Ok { v_db = PConfig.database; v_db_version }

let default_serv_response _comm_des _client_infos _infos error_desc =
  Db.get_version () >|= fun _ ->
  Ok
    (Utils.default_server_response_from_string _comm_des _client_infos _infos
       error_desc)

(* ****************************************************************** *)

(** Basic handler : get db version (default & test) *)
let version _params () =
  to_api
    ( Db.get_version () >|= fun v_db_version ->
      Ok { v_db = PConfig.database; v_db_version } )

(** Same as previous but with a json body in http request *)
let version_test_json_body _params elem =
  to_api
    ( Db.get_version () >|= fun v_db_version ->
      Ok { v_db = PConfig.database; v_db_version } )

(** Get job list from user with corresponding [Data_types.jobs_descr.job_client] : [elem.job_client]
    and [Data_types.jobs_descr.job_ref_tag] : [elem.job_ref_tag_v] *)
let sr_job_desc _params elem =
  to_api (Db.get_job_desc elem >|= fun jobs -> Ok jobs)

(** Retrieve all jobs from user specified in [req] *)
let get_all_jobs _params req =
  to_api (Db.get_all_jobs_from_user req >|= fun jobs -> Ok jobs)

(** Testing session ? ...  *)
let test_session (req, _arg) r =
  to_api
    ((* My_Session.connect req EzAPI.no_security *)
     (* connect is to be used client side atm *)
     My_Session.get_request_session req
     >>= function
     | Some { session_token; session_login; session_last; session_user_id; _ }
       ->
         Db.get_version () >|= fun v_db_version ->
         Ok
           {
             v_db =
               PConfig.database ^ "//" ^ session_token ^ "//" ^ session_login
               ^ "//" ^ session_user_id ^ "//"
               ^ string_of_float session_last;
             v_db_version;
           }
     | None ->
         Db.get_version () >|= fun v_db_version ->
         Ok { v_db = "bad"; v_db_version = -100 })

(** Signup new user with data provided in [Data_types.user_description] user .
    Initialize first login date to current_timestamp .
    Register (active) session for newly signed up user *)
let sign_up_new_user _params user =
  to_api
    (if not @@ Utils.check_email_validity user.email then (
     print_endline
     @@ Printf.sprintf "email invalid : regex invalid : {%s}" user.email;
     default_serv_response "attempt to create user"
       "client infos to be added here" "Error"
       "Signup Error : Bad email address")
    else if not @@ Utils.check_password_validity user.password then (
      print_endline
      @@ Printf.sprintf "password invalid : {%s} regex invalid" user.password;
      default_serv_response "attempt to create user"
        "client infos to be added here" "Error" "Signup Error : Bad password")
    else
      let user = hash_user_desc user in
      let _ = Db.add_user_to_db user in
      try
        Registered_Users.create_user ~password:user.password
          ~login:user.username user.user_desc;
        default_serv_response "attempt to create user"
          "client infos to be added here" "Ok" "Go check Db / verification"
      with
      | EzSessionServer.UserAlreadyDefined ->
          print_endline "User already defined";
          default_serv_response "attempt to create user"
            "client infos to be added here" "Error"
            "Signup Error : EzSessionServer.UserAlreadyDefined"
      | EzSessionServer.NoPasswordProvided ->
          print_endline "Please provide a decent password";
          default_serv_response "attempt to create user"
            "client infos to be added here" "Error"
            "Signup Error : EzSessionServer.NoPasswordProvided")

(* ****************************************************************** *)

let job_metadata _params meta_payload =
  to_api
    (let job_desc =
       {
         job_client = meta_payload.client_id;
         job_ref_tag = 0;
         order_ts = "fixed at insertion";
         path_to_f = root_files ^ Filename.basename meta_payload.archive_name;
         (* add uuid && || client username *)
         priority = meta_payload.priority;
         status = "scheduled";
       }
     in
     Db.insert_job job_desc >|= fun jobs -> Ok jobs)

(* ****************************************************************** *)

(* Websocket for zip transfer *)

(** Handles zip tranfert by retrieving the corresponding string
    sent through websocket : ws0 *)
let react_server_zip_ws0 _req _sec zip_archive =
  (* EzDebug.printf "server react : %s" zip_archive; *)
  retrieve_zip_from_string "/home/elias/Desktop/received_file.zip" zip_archive;
  Lwt.return_ok "echo from server"

(** Allows client to send zip through websocket : ws0 *)
let background_zip_ws0 _req _sec send =
  let bg =
    (* EzDebug.printf "server saying ok for receiving ZIP"; *)
    send @@ Ok "send me a zip";
    Lwt.return_unit
  in
  bg

(* ****************************************************************** *)

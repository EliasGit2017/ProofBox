open Lwt.Infix
open Data_types
open Db
open Bcrypt
(* open Services *)


(* ****************************************************************** *)

(* Redefine this in its own file when mastered *)

module SessionArg = struct
    type user_id = string
    type user_info = Data_types.user_info
    let user_id_encoding = Json_encoding.string
    let user_info_encoding = Encoding.user_info
    let rpc_path = []
    let token_kind = `CSRF "X-Csrf-Token" (* `Cookie "EZSESSION" *)
end


module Registered_Users = EzSessionServer.UserStoreInMemory(SessionArg)
module My_Session = EzSessionServer.Make(Registered_Users)

(* ****************************************************************** *)

let to_api_v0 p = Lwt.bind p EzAPIServerUtils.return
(** Initial to_api  *)

let to_api p =
    Lwt.catch
        (fun () -> Lwt.bind p EzAPIServerUtils.return)
        (fun err ->
            match err with
            | Proofbox_api_error exn -> EzAPIServerUtils.return ~code:500 (Error exn)
            | _ -> EzAPIServerUtils.return ~code:500 (Error Unknown))
(** [to_api p] executes [p] asynchroniously and encapsulates promise value to answer [EzAPIServerUtils.Answer.t].
    Catches and encapsulates server errors raised by [p] in order to get error's constructors
    [Data_types.server_error_type] from client-side. If [p] raises another type of error, then it is
    converted to [Unknown].*)



(* ****************************************************************** *)

let hash_bcrypt pre_hashed_password =
    Bcrypt.string_of_hash @@ Bcrypt.hash ~count:8 pre_hashed_password

let register_predefined_users user_d =
    Registered_Users.create_user 
      ~login:user_d.username
      ~password:(hash_bcrypt user_d.password)
      user_d.user_desc

let load_predefined_users =
    Db.get_all_users () >|= fun users ->
        let res = List.map Utils.users_to_string users in
        let _ = List.map print_endline res in
        List.map register_predefined_users users


(* ****************************************************************** *)


let version _params () = to_api (
    Db.get_version () >|= fun v_db_version ->
        Ok { v_db = PConfig.database; v_db_version })

let version_test_json_body _params elem = to_api (
    Db.get_version () >|= fun v_db_version ->
        Ok { v_db = PConfig.database; v_db_version })

let sr_job_desc _params elem = to_api (
    Db.get_job_desc elem >|= fun jobs ->
        Ok jobs
)

let get_all_jobs _params req = to_api (
    Db.get_all_jobs_from_user req >|= fun jobs ->
        Ok jobs
)

(** Testing session ? ...  *)
let test_session (req, _arg) r = to_api (
    (* My_Session.connect req EzAPI.no_security *)
    My_Session.get_request_session req >>= function
    | Some {session_token; session_login; session_last; session_user_id; _ } ->
        Db.get_version () >|= fun v_db_version ->
            Ok {v_db = PConfig.database ^ session_token ^ session_login ^ session_user_id ^ (string_of_float session_last); v_db_version}
    | None ->
        Db.get_version () >|= fun v_db_version ->
            Ok {v_db = "bad"; v_db_version = -100}
)



let sign_up_new_user (req, _arg) r = to_api (
    Registered_Users.create_user ~password:r.password ~login:r.username r.user_desc;
    print_endline @@ "These are the details of the signup : " ^ r.username ^ " ; " ^ r.email ^ " ; " ^ r.password ^ " ; "
    ^ r.user_desc ^ " ; " ^ r.first_login_date ^ " ; ";
    let _ = Db.add_user_to_db r in
    Db.get_version () >|= fun v_db_version ->
        Ok { v_db = PConfig.database; v_db_version }
    
)



open Data_types
open Bcrypt
open Utils
open Lwt.Infix

let api_port = ref 8080

let user1_login = "test_user60"

(* Bcrypt.string_of_hash @@ Bcrypt.hash ~count:7 "OcamlIsGood65!" *)

let user1_password = "dummydedada1234!"

let user1_info = "This user is also here for testing purposes"

let nrequests = ref 0

let waiting = ref false

let waiter,finalizer = Lwt.wait ()

let begin_request () = incr nrequests
let end_request () =
  decr nrequests;
  if !waiting && !nrequests = 0 then
    Lwt.wakeup finalizer ()

let waiting = ref false

(* let string_of_version t : Encoding.version -> string =
  Printf.sprintf "{ v_db = %S;\n  v_db_version = %S;\n}"
    t.name t.version *)

module SessionArg = struct
  type user_id = string
  type user_info = Data_types.user_info
  let user_id_encoding = Json_encoding.string
  let user_info_encoding = Encoding.user_info
  let rpc_path = []
  let token_kind = `CSRF "X-Csrf-Token" (* `Cookie "EZSESSION" *)
end

module Session = EzSessionClient.Make(SessionArg)
open EzAPI.TYPES

(* type login_error =
            [ `Bad_user_or_password
            | `Challenge_not_found_or_expired of user_info
            | `Invalid_session_connect of user_info
            | `Invalid_session_login of user_info
            | `Invalid_session_logout of user_info
            | `Session_expired
            | `Too_many_login_attempts
            | `Unverified_user
            | `User_not_registered ] *)

let to_str_err = function
          | `Bad_user_or_password -> "`Bad_user_or_password"
          | `Challenge_not_found_or_expired -> "`Challenge_not_found_or_expired"
          | `Invalid_session_connect -> "`Invalid_session_connect"
          | `Invalid_session_login -> "`Invalid_session_connect"
          | `Session_expired -> "`Invalid_session_connect"
          | `Too_many_login_attempts -> "`Invalid_session_connect"
          | `Unverified_user -> "`Invalid_session_connect"
          | `User_not_registered -> "`Invalid_session_connect"


 (* `Bad_user_or_password -> "`Bad_user_or_password"
| `Challenge_not_found_or_expired -> "`Challenge_not_found_or_expired"
| `Invalid_session_connect -> "`Invalid_session_connect"
| `Invalid_session_login ->  "`Invalid_session_login"
(* | `Invalid_session_logout -> "`Invalid_session_logout" *)
| `Session_expired -> "`Session_expired"
| `Too_many_login_attempts -> "`Too_many_login_attempts"
| `Unverified_user -> "`Unverified_user"
| `User_not_registered -> "`User_not_registered"
|`Unverified_user -> "`Unverified_user"
|`User_not_registered -> "`User_not_registered" *)

let proofbox_session_test arg api =
  print_endline "trying to connect";
  let open EzSession.TYPES in
  begin_request ();
  Session.connect
    api
    (function
    |Error _ ->
      Printf.eprintf "Error in connect\n%!";
      exit 2
    | Ok (Some _u) -> assert false
    | Ok None ->
      Session.login
        api
        ~login:user1_login ~password:user1_password
        (function
          | Error _ ->
            (* Printf.eprintf "%s\n%!" @@ Printexc.raw_backtrace_to_string @@ Printexc.get_callstack 100; *)
            Printf.eprintf "Error in login\n%!";
            exit 2
          | Ok u ->
            Printf.eprintf "auth login = %S\n%!" u.auth_login;
            Printf.eprintf "auth token = %S\n%!" u.auth_token;
            assert (u.auth_login = user1_login);
            assert (u.auth_user_info = user1_info);
            EzRequest.ANY.get0 ~msg:"version"
              api
              Services.version
              (* ~input:arg *)
              ~headers:(
                ("X-Another-Header2:", "x2") ::
                Session.auth_headers ~token:u.auth_token)
              (function
              |Ok r -> Printf.eprintf "test request to signup new user ... : return value --> %s\n%!"
                (version_test_to_string r);
                Session.logout
                  api
                  ~token:u.auth_token
                  (function
                    | Error _ ->
                      Printf.eprintf "Error in logout\n%!";
                      exit 2
                    | Ok bool ->
                      Printf.eprintf "logout OK %b\n%!" bool;
                      end_request ()
                  )
              | Error e ->
              Printf.eprintf "%s\n%!" @@ "This is an error from server, custom stuff";
              end_request ()
            )
        )
    )
    

let proofbox_session_test_json arg api =
  print_endline "trying to connect";
  let open EzSession.TYPES in
  begin_request ();
  Session.connect
    api
    (function
    |Error _ ->
      Printf.eprintf "Error in connect\n%!";
      exit 2
    | Ok (Some _u) -> assert false
    | Ok None ->
      Session.login
        api
        ~login:user1_login ~password:user1_password
        (function
          | Error _ ->
            (* Printf.eprintf "%s\n%!" @@ Printexc.raw_backtrace_to_string @@ Printexc.get_callstack 100; *)
            Printf.eprintf "Error in login\n%!";
            exit 2
          | Ok u ->
            Printf.eprintf "auth login = %S \nauth token = %S\n%!" u.auth_login u.auth_token;
            assert (u.auth_login = user1_login);
            assert (u.auth_user_info = user1_info);
            EzRequest.ANY.post0 ~msg:"version"
              api
              Services.version_test_json_body
              ~input:arg
              ~headers:(
                ("X-Another-Header2:", "x2") ::
                Session.auth_headers ~token:u.auth_token)
              (function
                |Ok r -> 
                  print_endline "In service rn";
                  Printf.eprintf "test request to get version using json body ... :\nreturn value --> %s\n%!"
                  (version_test_to_string r);
                  Session.logout
                    api
                    ~token:u.auth_token
                    (function
                      | Error _ ->
                        Printf.eprintf "Error in logout\n%!";
                        exit 2
                      | Ok bool ->
                        Printf.eprintf "logout OK %b\n%!" bool;
                        end_request ()
                    )
                | Error e ->
                  print_endline "no result returned";
                  Printf.eprintf "%s\n%!" @@ "This is an error from server, custom stuff";
                  end_request ()
            )
        )
    )
    

let () =
  Printexc.record_backtrace true;
  let api = Printf.sprintf "http://localhost:%d" !api_port in
  let api = BASE api in
  let requests = [
    (* proofbox_session_test {username = "test_ouser"; email = "azwbdklo@gmail.com"; password = "dummy1234!"; description = "This user is here for testing purposes"; first_login_date = "25-04-1997 20:45:30"}; *)
    (* proofbox_session_test {username = "test_user1"; email = "azwbdj@gmail.com"; password = "dummy1234!"; description = "This user is also here for testing purposes"; first_login_date = "25-04-1997 20:45:30"}; *)
    proofbox_session_test {username = "test_user60"; email = "azwbdjefegdsdaqdzar@gmail.com"; password = "dummydedada1234!"; description = "This user is also here for testing purposes"; first_login_date = "25-04-1997 20:45:30"};
    proofbox_session_test_json {basic = "okok"};
    ]
  in
  print_endline "Launching request Tests";
  List.iter (fun test -> test api) requests;
  if !nrequests > 0 then begin
    waiting := true;
    EzLwtSys.run (fun () -> waiter)
  end

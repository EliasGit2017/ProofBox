open Data_types
open Bcrypt
open Utils
open Lwt.Infix
open EzAPI
open EzAPI.TYPES

let api_port = ref 8080
let user1_login = "test_user60"
let user1_password = "dummydedada1234!"
let user1_info = "This user is also here for testing purposes"

(* lwt management *)
let waiter, finalizer = Lwt.wait ()
let waiting = ref false
let nrequests = ref 0
let begin_request () = incr nrequests

let end_request () =
  decr nrequests;
  if !waiting && !nrequests = 0 then Lwt.wakeup finalizer ()
(* // *)

type 'res response = ('res, server_error_type) result
(** Polymorphic response type from search-api. *)

(** Handler that converts [ez_api] error structure to [response] *)
let handle_error err =
  Lwt.return
  @@
  match err with
  | EzReq_lwt_S.UnknownError _ -> Error Unknown
  | EzReq_lwt_S.KnownError { error; _ } -> Error error

let handle_response (resp : 'res) : 'res response Lwt.t = Lwt.return @@ Ok resp
(* Data examples *)

let user_test1 =
  {
    username = "test_user0";
    email = "azwbdklo@gmail.com";
    password = "dummy1234!";
    user_desc = "This user is here for testing purposes";
    first_login_date = "25-04-1997 20:45:30";
  }

let user_test2 =
  {
    username = "test_user1";
    email = "azwbdj@hotmail.com";
    password = "dummy1234!";
    user_desc = "This user is also here for testing purposes";
    first_login_date = "25-04-1997 20:45:30";
  }

let user_test3 =
  {
    username = "test_user3";
    email = "example@gmail.com";
    password = "dummydedada1234!";
    user_desc = "This user is also here for testing purposes";
    first_login_date = "25-04-1997 20:45:30";
  }

let test_user4 =
  {
    username = "test_user2";
    email = "elias.ben@gmail.com";
    password = "Testing123%!";
    user_desc = "User added from postman";
    first_login_date = "irrelevant";
  }

let user_test_tyler =
  {
    username = "tyler";
    email = "tyler_durden@gmail.com";
    password = "2ndRulerefRule1$!";
    user_desc = "3rd user for testing only";
    first_login_date = "2022-08-07 14:45:52.523274";
  }

let user_test_marla =
  {
    username = "marla";
    email = "marla1991@hotmail.fr";
    password = "Rule1$!%";
    user_desc = "Here for testing only";
    first_login_date = "2022-08-07 14:45:52.523274";
  }

let user_test_james =
  {
    username = "james";
    email = "james.dean@gmail.com";
    password = "examPlePass1!";
    user_desc = "test user 1";
    first_login_date = "2022-08-07 14:45:52.523274";
  }

let default_users_list = [ user_test1; user_test2; user_test3 ]

(** Default error handler *)
let default_error_handler err =
  (match err with
  | Invalid_request -> print_endline "Client got : Invalid request"
  | No_sources_config -> print_endline "Client got : No config for sources"
  | Unknown -> print_endline "Client got : Unknown error");
  Lwt.return_unit

(** [send_generic_request ~request ~callback ~error] executes function that sends request [request] to search-api
    and execute continuations according to the request result. When request is succesfull [callback] is applied 
    on response. Otherwise, [error] is applied on an [Data_types.server_error]. If no [error] argument was specified -
    [default_error_handler] is used.*)
let send_generic_request :
      'res.
      request:(unit -> 'res response Lwt.t) ->
      callback:('res -> unit Lwt.t) ->
      ?error:(server_error_type -> unit Lwt.t) ->
      unit ->
      unit Lwt.t =
 fun ~request ~callback ?(error = default_error_handler) () ->
  let%lwt resp = request () in
  match resp with Ok res -> callback res | Error err -> error err

let error test n =
  Printf.eprintf "Error: request %s returned code %d\n%!" test n;
  exit 2

let basic api =
  begin_request ();
  EzRequest.ANY.get0 (* EzReq_lwt.get0 *)
    ~msg:"simplest req possible" api Services.version
    (* ~error:(error "even on the dummiest") *) (function
    | Ok r ->
        Printf.eprintf "Result for Simple req : %s\n%!"
          (Utils.version_test_to_string r);
        (* handle_response r *)
        end_request ()
    | Error e ->
        Printf.eprintf "%s\n%!" @@ "very bad error even in basic req";
        (* handle_error e *)
        end_request ())

let base_req arg api =
  print_endline "sending req ==> base req";
  EzRequest_lwt.ANY.post0 ~msg:"trying simple req" ~input:arg api
    Services.version_test_json_body
  >>= (* ~error:(error "base_req")  *)
  function
  | Ok e ->
      (* print_endline "ok got res"; *)
      Printf.eprintf "getting db version %s\n%!"
        (Utils.version_test_to_string e);
      (* handle_response e  *)
      Lwt.return_unit
  | Error e ->
      print_endline "bad error";
      (* Printf.eprintf "%s\n%!" @@ Printexc.to_string (proofbox_api_error e); *)
      (* handle_error e *)
      Lwt.return_unit

let base_req2 arg api =
  begin_request ();
  print_endline "sending ==> base_req2";
  EzRequest.ANY.post0 ~msg:"test3" api Services.version_test_json_body
    ~error:(error "error classic req; no lwt //") ~input:arg (function
    | Ok r ->
        Printf.eprintf "Test test3 returned %s\n%!"
          (Utils.version_test_to_string r);
        end_request ()
    | Error e ->
        Printf.eprintf "%s\n%!" @@ Printexc.to_string (proofbox_api_error e);
        end_request ())

let () =
  Printexc.record_backtrace true;
  let api = Printf.sprintf "http://localhost:%d" !api_port in
  print_endline ("sending req 0 to " ^ api);
  let api = BASE api in
  let requests =
       [ (* base_req2 { basic = "okok" }; base_req2 { basic = "okok" } *) basic; ]
     in
     List.iter (fun test -> test api) requests;
     if !nrequests > 0 then (
       waiting := true;
       EzLwtSys.run (fun () -> waiter));
  (* Lwt.async
  @@ send_generic_request
       ~request:(fun () -> basic api)
       ~callback:(fun res ->
         print_endline (Utils.version_test_to_string res);
         Lwt.return_unit); *)

  Lwt.async @@ fun () -> base_req { basic = "okok" } api

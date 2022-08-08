open Data_types

(** Catches DB excetpions and raises [Data_types.Proofbox_api_error] coresponding to DB error *)
let catch_db_error f =
  Lwt.catch f @@ fun err ->
  Printf.eprintf "catch_db_error\n";
  flush stderr;
  match err with
  | Db_lwt.PGOCaml.PostgreSQL_Error _ ->
      Printf.eprintf "PostgreSQL_Error\n";
      flush stderr;
      Lwt.fail @@ proofbox_api_error Invalid_request
  | exn ->
      Printf.eprintf "Another error : %s\n" (Printexc.to_string exn);
      flush stderr;
      Lwt.fail @@ proofbox_api_error Unknown

(* ****************************************************************** *)

(** Example : used for v_db_version  *)
let version_of_rows = function [ Some v ] -> Int32.to_int v | _ -> 0

(** Creates [Data_types.jobs_descr] from DB jobs_description table rows *)
let jobs_of_rows rows =
  List.map
    (function
      | row ->
          {
            job_client = row#job_client;
            job_ref_tag = Int32.to_int row#job_id;
            order_ts = CalendarLib.Printer.Calendar.to_string row#order_ts;
            path_to_f = row#path_to_f;
            priority = Int32.to_int row#priority;
            status = row#status;
          })
    rows

(** Creates [Data_types.user_description] from DB users table rows *)
let users_of_rows rows =
  List.map
    (function
      | row ->
          {
            username = row#username;
            email = row#email;
            password = row#password;
            user_desc = row#user_desc;
            first_login_date =
              CalendarLib.Printer.Calendar.to_string row#first_login_date;
          })
    rows

let one_user row =
  {
    username = row#username;
    email = row#email;
    password = row#password;
    user_desc = row#user_desc;
    first_login_date =
      CalendarLib.Printer.Calendar.to_string row#first_login_date;
  }

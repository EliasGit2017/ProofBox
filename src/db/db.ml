open Lwt.Infix
open Misc_db
open Db_lwt
open Data_types

(** Simple Db request : used for debug/verbose purposes *)
let get_version () =
  with_dbh >>> fun dbh ->
  [%pgsql dbh "select value from ezpg_info where name = 'version'"]
  >|= version_of_rows

(** Retrieve job description to job having [job_client] and [job_id]
    corresponding to the fields specified in [Data_types.job_desc_req] *)
let get_job_desc {job_client; job_ref_tag_v} =
  with_dbh >>> fun dbh -> catch_db_error @@
    fun () ->
      [%pgsql.object dbh "select *
                          from jobs_description
                          where job_client = $job_client and job_id = ${Int32.of_int job_ref_tag_v}"]
      >|= jobs_of_rows


(** Retrieve all jobs from user having username specified in
    [Data_types.all_jobs_get.job_client_req] . (TO DO) *)
let get_all_jobs_from_user {job_client_req} =
  with_dbh >>> fun dbh -> catch_db_error @@
    fun () ->
      [%pgsql.object dbh "select *
                          from jobs_description
                          where job_client = $job_client_req"]
      >|= jobs_of_rows

let get_all_jobs_from_userstring (job_client_req : string) =
  with_dbh >>> fun dbh -> catch_db_error @@
    fun () ->
      [%pgsql.object dbh "select *
                          from jobs_description
                          where job_client = $job_client_req"]
      >|= jobs_of_rows


(** Add [Data_types.user_description] user to DB table [users] with
    [first_login_date] field set to current timestamp (current_timestamp in
    psql) *)
let add_user_to_db {username; email; password; user_desc; _} =
  let fld_to_caltype = CalendarLib.Calendar.now () in
  with_dbh >>> fun dbh -> catch_db_error @@
    fun () ->
      [%pgsql dbh "INSERT INTO users (username, email, password, user_desc, first_login_date)
                   VALUES ($username, $email, $password, $user_desc, $fld_to_caltype)"]

(** Retrieve all users from DB table [users] *)
let get_all_users () =
  with_dbh >>> fun dbh -> catch_db_error @@
    fun () ->
      [%pgsql.object dbh "select * from users"]
      >|= users_of_rows


let get_user {username; _} =
  with_dbh >>> fun dbh -> catch_db_error @@
  fun () ->
    [%pgsql.object dbh "select *
                        from users
                        where username = $username"] 

    >|= users_of_rows

(** insert job from metadata and retrieve all jobs in db from the client who
  initiated the zip transfer *)
let insert_job {job_client; path_to_f; priority; status; _} =
  let fld_to_caltype = CalendarLib.Calendar.now () in
  with_dbh >>> fun dbh -> catch_db_error @@
  fun () ->
    [%pgsql dbh "insert INTO jobs_description
    (job_client, order_ts, path_to_f, priority, status) 
    VALUES ($job_client, $fld_to_caltype, $path_to_f, ${Int32.of_int priority}, $status)"];
  get_all_jobs_from_userstring job_client
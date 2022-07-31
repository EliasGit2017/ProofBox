open Lwt.Infix
open Misc_db
open Db_lwt
open Data_types

let get_version () =
  with_dbh >>> fun dbh ->
  [%pgsql dbh "select value from ezpg_info where name = 'version'"]
  >|= version_of_rows

let get_job_desc {job_client; job_ref_tag_v} =
  with_dbh >>> fun dbh -> catch_db_error @@
    fun () ->
      [%pgsql.object dbh "select *
                   from jobs_description
                   where job_client ~* $job_client and job_id ~* $job_ref_tag_v"]
      >|= jobs_of_rows
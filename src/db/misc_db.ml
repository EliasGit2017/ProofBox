open Data_types

let version_of_rows = function [ Some v ] -> Int32.to_int v | _ -> 0

let catch_db_error f =
  Lwt.catch f
  @@ (fun err ->
      Printf.eprintf "catch_db_error\n"; flush stderr;
      match err with
      | Db_lwt.PGOCaml.PostgreSQL_Error _ -> (Printf.eprintf "PostgreSQL_Error\n"; flush stderr;
      Lwt.fail @@ proofbox_api_error Invalid_request)
      | exn -> Printf.eprintf "Another error : %s\n" (Printexc.to_string exn);
          flush stderr;Lwt.fail @@ proofbox_api_error Unknown)
(** Catches DB excetpions and raises [Data_types.Proofbox_api_error] coresponding to DB error *)

let jobs_of_rows rows =
  List.map (function row ->
    {job_client = row#job_client;
    job_ref_tag = Int32.to_int row#job_id;
    order_ts = row#order_ts;
    path_to_f = row#path_to_f;
    priority = Int32.to_int row#priority;
    status = row#status}
    ) rows
(** Creates [Data_types.jobs_descr] from DB jobs_description table *)
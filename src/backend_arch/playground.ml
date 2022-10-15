open Lwt.Infix
open Data_types
open Db
open EzPG
open PGOCaml

let job_list_to_string job_l =
  List.fold_left
    (fun res { job_client; job_ref_tag; order_ts; path_to_f; priority; status } ->
      res
      ^ Printf.sprintf
          "{ job_client = %s; job_ref_tag = %d; order_ts = %s; path_to_f =%s ; \
           priority = %d; status = %s }\n"
          job_client job_ref_tag order_ts path_to_f priority status)
    "" job_l

let empty_job_desc =
  {
    job_client = "";
    job_ref_tag = 0;
    order_ts = "";
    path_to_f = "";
    checksum_type = "";
    checksum = "";
    priority = 0;
    status = "";
  }

let consult_jobs () =
  let%lwt res = Db.get_jobs () in
  Lwt.return res


let () =
  Lwt_main.run @@ 
  let%lwt res = consult_jobs () in
  print_endline "printing current jobs to be done";
  print_endline (Printf.sprintf "%s" (job_list_to_string @@ res));
  Lwt.return_unit




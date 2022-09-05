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

let consult_jobs ?(verbose = false) () =
  let%lwt res = Db.get_jobs () in
  if verbose then
    print_endline
      (Printf.sprintf "Printing job in second promise : %s"
         (job_list_to_string res));
  Lwt.return_unit

(* let () =
   Lwt_main.run
   @@
   let _ = consult_jobs () in
   Lwt.return_unit *)

open Lwt.Infix
open Data_types
open Db
open Toml_reader.Utils

let server_job_manager_status = ref "Idle"

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

(** Sample function for debug/testing purposes only *)
let consult_jobs ?(verbose = false) () =
  let%lwt res = Db.get_jobs () in
  if verbose then
    print_endline
      (Printf.sprintf "Printing job in second promise : %s"
         (job_list_to_string res));
  Lwt.return_unit

(** Retrieve all the jobs available in db when called *)
let jobs_todo () =
  let res = Db.get_jobs () in
  res

let rec scheduler_main_loop () =
  let%lwt todo_list = jobs_todo () in
  if List.length todo_list <= 2 then (
    server_job_manager_status := "Idle";
    Lwt_unix.sleep 5.)
  else
    let task_to_solve = List.hd @@ todo_list in
    print_endline
      (Printf.sprintf "In scheduler main loop : \n %s"
         (job_list_to_string [ task_to_solve ]));
    (* deflate  ---> check if not already deflated*)
    Tools.deflate_zip_archive task_to_solve.path_to_f
      (Filename.dirname task_to_solve.path_to_f);
    print_endline (Printf.sprintf "%s" (Filename.dirname task_to_solve.path_to_f ^ "/results/"));
      (* parse / read toml *)
    let toml_spec =
      retrieve_toml_values
        (Filename.dirname task_to_solve.path_to_f ^ "/results/")
    in
    ht_printer toml_spec;
    (* send to docker arch *) (* Timeout is needed *)
    scheduler_main_loop ()

(* let () =
   Lwt_main.run
   @@
   let _ = consult_jobs () in
   Lwt.return_unit *)

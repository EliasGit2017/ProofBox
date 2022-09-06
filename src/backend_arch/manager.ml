open Lwt.Infix
open Data_types
open Db
open Utils
open Tools
open Toml_reader.Utils

let server_job_manager_status = ref "Idle"

let available_c_of_sol =
  ref 5 (* nb of available containers for one solver style *)

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

(** Main job solving event loop -> to optimize base conditions + make sure rec iterations *)
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
    let working_dir = Filename.dirname task_to_solve.path_to_f ^ "/unzipped/" in
    (* deflate  ---> check if not already deflated*)
    deflate_zip_archive task_to_solve.path_to_f working_dir;
    (* parse / read toml *)
    let toml_spec = retrieve_toml_values working_dir in
    ht_printer toml_spec;
    (* Manage docker arch (scale if > 10) & send to docker arch --> delete scaled containers ? *)
    let files = dir_contents working_dir in
    List.iter
      (fun x -> print_endline (Printf.sprintf "%s" (Tools.opt_l_tostring x)))
      (cmds_builder toml_spec files !available_c_of_sol);
    (* Timeout is needed *)
    scheduler_main_loop ()

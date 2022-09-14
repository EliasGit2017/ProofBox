open Lwt.Infix
open Data_types
open Db
open Utils
open Tools
open Toml_reader.Utils
open Parmap
open Misc

(* server status *)
let server_job_manager_status = ref "Idle"

(* nb of available containers for one type of solver *)
let available_c_of_sol = ref 1

(* List representing indexes of available containers *)
let indexes_c_of_sol = ref (list_range 1 !available_c_of_sol)

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

let scale_arch (file_l : string list) (solver : string) (version : string)
    (nb_dup : int) =
  (* don't count toml file --> don't run solver on toml file *)
  if List.length file_l > 10 then (
    let status_code =
      Sys.command
        (Printf.sprintf
           "docker-compose -f \
            /home/elias/OCP/ez_proofbox/scripts/Containers/Docker_arch/docker-compose.yml \
            --compatibility  up --scale %s=%d -d"
           (solver ^ "-" ^ version)
           nb_dup)
    in
    available_c_of_sol := nb_dup;
    indexes_c_of_sol := list_range 1 !available_c_of_sol;
    print_endline (Printf.sprintf "Exit status code : %d" status_code))
  else print_endline "No need to scale; not enough files"

(** Main job solving event loop -> to optimize base conditions + make sure rec iterations *)
let rec scheduler_main_loop () =
  let%lwt todo_list = jobs_todo () in
  if List.length todo_list = 0 then (
    server_job_manager_status := "Idle";
    print_endline "waiting for a reason to exist";
    let _ = Lwt_unix.sleep 5. in
    scheduler_main_loop ())
  else
    let task_to_solve = List.hd @@ todo_list in
    print_endline
      (Printf.sprintf "In scheduler main loop : \n %s"
         (job_list_to_string [ task_to_solve ]));
    (* set server status to [Working] *)
    server_job_manager_status := "Working";
    let working_dir = Filename.dirname task_to_solve.path_to_f ^ "/unzipped/" in
    (* deflate  ---> check if not already deflated*)
    deflate_zip_archive task_to_solve.path_to_f working_dir;
    (* parse / read toml *)
    let toml_spec = retrieve_toml_values working_dir in
    (* print Hashtable storing job options *)
    ht_printer toml_spec;
    let files = dir_contents working_dir in
    let real_path_for_container =
      List.map
        (fun x ->
          let decomp = String.split_on_char '/' x in
          let l_length = List.length decomp in
          String.concat "/" (sublist ~start:(l_length - 3) 3 decomp))
        files
    in
    (* List.iter
       (fun x -> print_endline (Printf.sprintf "%s" (Tools.opt_l_tostring x)))
       (cmds_builder toml_spec real_path_for_container available_c_of_sol); *)
    scale_arch real_path_for_container
      (Hashtbl.find toml_spec "jd_solver")
      (Hashtbl.find toml_spec "jd_solver_version")
      !Tools.nb_duplicates;
    let all_cmds =
      cmds_builder toml_spec real_path_for_container available_c_of_sol
    in
    (* build result dir *)
    (if
     not
       (Sys.file_exists
       @@ Filename.dirname task_to_solve.path_to_f
       ^ "/results/")
    then
     let _ =
       Sys.command
         ("mkdir -p " ^ Filename.dirname task_to_solve.path_to_f ^ "/results/")
     in
     ()
    else
      let _ =
        Sys.command
          ("rm " ^ Filename.dirname task_to_solve.path_to_f ^ "/results/*.txt")
      in
      ());
    (* run jobs *)
    pariter ~ncores:5 run_cmd (L all_cmds);
    (* update dbs *)
    print_endline (Printf.sprintf "%s" task_to_solve.path_to_f);
    let%lwt _ = Db.job_done task_to_solve.path_to_f in
    let%lwt _ =
      Db.update_cache task_to_solve.job_ref_tag
        (Filename.dirname task_to_solve.path_to_f ^ "/results/")
        "solved"
    in
    (* send mail *)
    (* scale down *)
    scale_arch real_path_for_container
      (Hashtbl.find toml_spec "jd_solver")
      (Hashtbl.find toml_spec "jd_solver_version")
      1;
    scheduler_main_loop ()

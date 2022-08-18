open Otoml
open Read_write_toml.Utils

let baseTOML_dir = "/home/elias/OCP/PROOFBOX_TestJobs/job_example1/"

let get_main_toml =
  let res = get_all_files_w_ext baseTOML_dir ".toml" in
  if List.length res = 0 then (
    print_endline "Start thinking about errors and exceptions";
    (* Throw exception here *)
    "No TOML found")
  else baseTOML_dir ^ List.hd res
(* get absolute path through Sys ? *)

let retrieve_toml_values
    (* ~file *)
    (* : Otoml.t *) =
  let parsed_toml = Otoml.Parser.from_file get_main_toml in
  let htab_toml_values = Hashtbl.create 10 in
  try
    Hashtbl.add htab_toml_values "title" (get_title parsed_toml);
    Hashtbl.add htab_toml_values "owner_username"
      (get_owner_username parsed_toml);
    Hashtbl.add htab_toml_values "owner_email" (get_owner_email parsed_toml);
    Hashtbl.add htab_toml_values "owner_bio" (get_owner_bio parsed_toml);
    Hashtbl.add htab_toml_values "owner_password"
      (get_owner_password parsed_toml);
    Hashtbl.add htab_toml_values "jd_job_id"
      (string_of_int @@ get_jd_job_id parsed_toml);
    Hashtbl.add htab_toml_values "jd_solver" (get_jd_solver parsed_toml);
    Hashtbl.add htab_toml_values "jd_solver_version"
      (get_jd_solver_version parsed_toml);
    Hashtbl.add htab_toml_values "jd_synopsis" (get_jd_job_synopsis parsed_toml);
    Hashtbl.add htab_toml_values "jd_path_to_client_repo"
      (get_jd_path_tof parsed_toml);
    htab_toml_values
  with Type_error _ | Key_error _ ->
    print_endline @@ Printf.sprintf "Badly formatted TOML at : %s" get_main_toml;
    Hashtbl.create 0

let () =
  Printexc.record_backtrace true;
  let test = Otoml.Parser.from_file (path_to_toml ^ "/job.toml") in
  test
  |> Otoml.Printer.to_channel ~indent_width:4 ~indent_subtables:true
       ~collapse_tables:false stdout;
  print_endline "\n ==> getting access to nested +/- values\n";

  print_endline "=> email : ";
  Otoml.find test Otoml.get_value [ "owner"; "email" ]
  |> Otoml.Printer.to_channel stdout;

  print_endline "\n=> job_description table : ";
  get_owner_bio test (* |> Otoml.Printer.to_string *) |> print_endline;

  print_endline "=> list files ==> :";
  retrieve_toml_files path_to_toml;
  let jdptof = get_str test [ "job_description"; "path_to_client_repo" ] in
  (* stringlist_printer
  @@ get_all_files_w_ext_smts
       "/home/elias/OCP/PROOFBOX_TestJobs/job_example1/ALIA/piVC"; *)
  stringlist_printer @@ dir_contents jdptof;
  print_endline @@ Printf.sprintf "\nUnix.getcwd : %s" (testunix ());
  print_endline "==> Main TOML : =>";
  print_endline get_main_toml;
  if Hashtbl.length retrieve_toml_values = 0 then
    print_endline "badly formatted toml"
  else
    Hashtbl.iter
      (fun x y -> print_endline @@ Printf.sprintf "%s %s" x y)
      retrieve_toml_values
(* print_endline @@ Printf.sprintf "%d" (List.length @@ get_all_files_w_ext baseTOML_dir ".toml") *)

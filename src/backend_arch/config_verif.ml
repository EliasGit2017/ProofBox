open Otoml
open Read_write_toml.Utils

let baseTOML_dir = "/home/elias/OCP/PROOFBOX_TestJobs/job_example1"

let get_main_toml =
  let res = get_all_files_w_ext baseTOML_dir ".toml" in
  if List.length res = 0 then (
    print_endline "Start thinking about errors and exceptions"; (* Throw exception here *)
    "No TOML found")
  else List.hd res


  
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

  (* print_endline "=> list files ==> :";
  retrieve_toml_files path_to_toml;
  let jdptof = get_str test [ "job_description"; "path_to_client_repo" ] in
  stringlist_printer
  @@ get_all_files_w_ext_smts
       "/home/elias/OCP/PROOFBOX_TestJobs/job_example1/ALIA/piVC";
  stringlist_printer @@ dir_contents jdptof;
  print_endline @@ Printf.sprintf "Unix.getcwd : %s" (testunix ()) *)
  print_endline "==> Main TOML : =>";
  print_endline get_main_toml;
  (* print_endline @@ Printf.sprintf "%d" (List.length @@ get_all_files_w_ext baseTOML_dir ".toml") *)

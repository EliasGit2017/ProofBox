open Otoml
open Read_write_toml.Utils

let path_to_toml = "/home/elias/OCP/ez_proofbox/src/backend_arch"
let testunix = Unix.getcwd

let get_all_files_w_ext wd =
  Sys.readdir wd |> Array.to_list
  |> List.filter (fun x -> Filename.extension x = ".smt2")

(** [dir_contents] returns the paths of all regular files that are
 * contained in [dir]. Each file is a path starting with [dir].
  *)
let dir_contents dir =
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
        Sys.readdir f |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs |> loop result
    | f :: fs -> loop (f :: result) fs
    | [] -> result
  in
  loop [] [ dir ]

let retrieve_toml_files base_dir =
  let ((ocaml_stdout, ocaml_stdin, ocaml_stderr) as p) =
    Unix.open_process_args_full "/usr/bin/ls"
      [| "/usr/bin/ls"; "/home/elias/OCP/ez_proofbox/src/backend_arch" |]
      (Unix.environment ())
  in
  let l_res = ref [] in
  (* print_endline @@ Printf.sprintf "%d" @@ List.length @@ chan_to_stringlist ocaml_stdout;
     print_endline @@ Printf.sprintf "%d" @@ List.length @@ chan_to_stringlist ocaml_stderr; *)
  stringlist_printer @@ chan_to_stringlist ocaml_stdout;
  stringlist_printer @@ chan_to_stringlist ocaml_stderr;

  print_endline "printing my list";
  stringlist_printer !l_res;

  let stat = Unix.close_process_full p in
  print_endline @@ stat_code stat

let () =
  Printexc.record_backtrace true;
  let test =
    Otoml.Parser.from_file
      "/home/elias/OCP/ez_proofbox/src/backend_arch/job.toml"
  in
  test
  |> Otoml.Printer.to_channel ~indent_width:4 ~indent_subtables:true
       ~collapse_tables:false stdout;
  print_endline "\n ==> getting access to nested +/- values\n";

  print_endline "=> email : ";
  Otoml.find test Otoml.get_value [ "owner"; "email" ]
  |> Otoml.Printer.to_channel stdout;

  print_endline "\n=> job_description table : ";
  get_owner_bio test |> Otoml.Printer.to_string |> print_endline;

  print_endline " list files";
  retrieve_toml_files path_to_toml;
  let jdptof =
    get_str test [ "job_description"; "path_to_client_repo" ]
    (* |> Otoml.Printer.to_string *)
  in
  stringlist_printer
  @@ get_all_files_w_ext
       "/home/elias/OCP/PROOFBOX_TestJobs/job_example1/ALIA/piVC";
  stringlist_printer @@ dir_contents jdptof

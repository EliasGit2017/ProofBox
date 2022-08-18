open Otoml
open Read_write_toml.Utils

let print_chan channel =
  let rec loop () =
    let () = print_endline (input_line channel) in
    loop ()
  in
  try loop () with End_of_file -> close_in channel

let retrieve_toml_files base_dir =
  let (ocaml_stdout, ocaml_stdin, ocaml_stderr) =
    Unix.open_process_args_full "ls"
      [| "/usr/bin/ls"; base_dir; (* "|"; "grep"; "*.toml" *) |]
      (Unix.environment ())
  in
  close_out ocaml_stdin;
  print_chan ocaml_stdout;
  print_chan ocaml_stderr

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
  (* retrieve_toml_files "/home/elias/OCP/ez_proofbox/src/backend_arch" *)
  let (ocaml_stdout, ocaml_stdin, ocaml_stderr) as p =
    Unix.open_process_args_full "ls"
      [| "/usr/bin/ls"; "/home/elias/OCP/ez_proofbox/src/backend_arch"; (* "|"; "grep"; "*.toml" *) |]
      (Unix.environment ())
  in
  print_chan ocaml_stdout;
  print_chan ocaml_stderr;
  let stat = Unix.close_process_full p in
  print_endline @@ stat_code stat

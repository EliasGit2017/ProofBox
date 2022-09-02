let chan_to_stringlist channel =
  let rec loop acc =
    try loop (input_line channel :: acc) with End_of_file -> List.rev acc
  in
  loop []

(** Print string list with [print_endline] for each element *)
let rec stringlist_printer = function
  | [] -> ()
  | e :: l ->
      print_endline e;
      stringlist_printer l

let launch_process cmd =
  let ((ocaml_stdout, ocaml_stdin, ocaml_stderr) as p) =
    Unix.open_process_full cmd (Unix.environment ())
  in
  stringlist_printer @@ chan_to_stringlist ocaml_stdout;
  stringlist_printer @@ chan_to_stringlist ocaml_stderr

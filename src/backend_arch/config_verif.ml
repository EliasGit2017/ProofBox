open Toml

(* let error_printer  *)

let main () =
  print_endline "Running toml parsing/reading tests"
  let parse_exp = Toml.Parser.from_string "key = [1,2]";;
  match parse_exp with
  | `Ok e -> print_endline @@ Toml.Printer.string_of_table e
  | `Error _ -> print_endline "error in parsing; to do elaborate"
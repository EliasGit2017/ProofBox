open Otoml
open Read_write_toml

let () =
  Printexc.record_backtrace true;
  let test =
    Otoml.Parser.from_file
      "/home/elias/OCP/ez_proofbox/src/backend_arch/job.toml"
  in
  test
  |> Otoml.Printer.to_channel ~indent_width:4 ~indent_subtables:true
       ~collapse_tables:false stdout;
  print_endline "\n ==> getting access to nested +/- values\n =>";
  print_endline "=> email : ";
  Otoml.find test Otoml.get_value [ "owner"; "email" ]
  |> Otoml.Printer.to_channel stdout;
  print_endline "=> job_description table : ";
  Utils.get_owner_bio test |> Otoml.Printer.to_string |> print_endline
open Printf
open Docker
open Parmap
module C = Docker.Container
module T = Docker.Tools

type opt_l = { cmd : string; opts : string list }

let l =
  [
    { cmd = "docker_arch_alt-ergo-2.3.3_1"; opts = [ "pwd" ] };
    { cmd = "docker_arch_alt-ergo-2.3.2_1"; opts = [ "pwd" ] };
    { cmd = "docker_arch_alt-ergo-2.3.0_1"; opts = [ "pwd" ] };
    { cmd = "docker_arch_alt-ergo-2.3.1_1"; opts = [ "pwd" ] };
    { cmd = "docker_arch_alt-ergo-2.4.1_1"; opts = [ "pwd" ] };
    { cmd = "docker_arch_alt-ergo-2.4.0_1"; opts = [ "pwd" ] };
  ]

let run_cmd (cmds : opt_l) =
  let dlc = C.list ~all:false ~size:true () in
  let e =
    C.Exec.create
      (T.ids_from_containers_list_wname ("/" ^ cmds.cmd) dlc)
      cmds.opts
    (* [ "alt-ergo-2.4.1"; "-vp"; "ALIA/piVC/piVC_030ee9.smt2" ] *)
  in
  let st = C.Exec.start e in
  let s = Docker.Stream.read_all st in
  let identify (ty, s) =
    match ty with
    | Docker.Stream.Stdout -> "out> " ^ s
    | Docker.Stream.Stderr -> "err> " ^ s
  in
  (* printf "Exec in the container returned:\n%s\n"
    (String.concat "\n" (List.map identify s)); *)
  let oc = open_out ("/home/elias/OCP/ez_proofbox/writters_draft/res" ^ cmds.cmd ^ ".txt") in
  output_string oc (String.concat "\n" (List.map identify s));
  close_out oc

let () =
  Printexc.record_backtrace true;
  print_endline
    (Printf.sprintf
       "default ncores : %d\ncores being used durint // execution : %d\n"
       (get_default_ncores ()) (get_ncores ()));
  (* run_cmd @@ List.hd l; *)
  pariter ~ncores:5 run_cmd (L l)

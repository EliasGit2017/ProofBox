open Zip
open Printf
open Docker
open Parmap
open Utils
module C = Docker.Container
module T = Docker.Tools

type opt_l = { cmd : string; opts : string list }

let opt_l_maker command options = { cmd = command; opts = options }

let opt_l_tostring (o : opt_l) =
  sprintf "{ cmd = %s; opts = %s }" o.cmd (stringlist_tostring " " o.opts)

let l =
  [
    { cmd = "docker_arch_alt-ergo-2.3.3_1"; opts = [ "ls"; "-a" ] };
    { cmd = "docker_arch_alt-ergo-2.3.2_1"; opts = [ "ls"; "-a" ] };
    { cmd = "docker_arch_alt-ergo-2.3.0_1"; opts = [ "ls"; "-a" ] };
    { cmd = "docker_arch_alt-ergo-2.3.1_1"; opts = [ "ls"; "-a" ] };
    { cmd = "docker_arch_alt-ergo-2.4.1_1"; opts = [ "ls"; "-a" ] };
    { cmd = "docker_arch_alt-ergo-2.4.0_1"; opts = [ "ls"; "-a" ] };
  ]

let cmds_builder (toml_ht : (string, string) Stdlib__hashtbl.t)
    (files_l : string list) (max_containers_available : int) =
  let all_cmds = ref [] in
  let solver = Hashtbl.find toml_ht "jd_solver" in
  let solver_version = Hashtbl.find toml_ht "jd_solver_version" in
  List.iter
    (fun x ->
      (* acc *)
      (* think about scaling :: unscale with uptime & container status ? list management *)
      all_cmds :=
        {
          cmd =
            "docker_arch_" ^ solver ^ "-" ^ solver_version ^ "_"
            ^ string_of_int (Random.int max_containers_available + 1)
            (* rand int -> switch to list managment *);
          opts = [ solver ^ "-" ^ solver_version; "-vp"; x ];
        }
        :: !all_cmds)
    files_l;
  !all_cmds

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
  let oc =
    Stdlib.open_out
      ("/home/elias/OCP/ez_proofbox/writters_draft/res" ^ cmds.cmd ^ ".txt")
  in
  output_string oc (String.concat "\n" (List.map identify s));
  Stdlib.close_out oc

(** deflate zip to result directory  : TO_OPT *)
let deflate_zip_archive zip_arch_name dest =
  (* let dest_dir = dest ^ "/results/" in *)
  let _ = Sys.command ("mkdir -p " ^ dest) in
  let in_f = open_in zip_arch_name in
  let arch_entries = entries in_f in
  List.iter
    (fun x -> copy_entry_to_file in_f x (dest ^ x.filename))
    arch_entries

open Zip
open Printf
open Docker
open Parmap
open Utils
module C = Docker.Container
module T = Docker.Tools

(** dest where result files will be written *)
let storage_dir = "/home/elias/OCP/ez_proofbox/scripts/Containers/storage"

(* nb of services to duplicate when scaling *)
let nb_duplicates = ref 8

type opt_l = { cmd : string; opts : string list }

let opt_l_maker command options = { cmd = command; opts = options }

let opt_l_tostring (o : opt_l) =
  sprintf "{ cmd = %s; opts = %s }" o.cmd (stringlist_tostring " " o.opts)

(** example *)
let l =
  [
    { cmd = "docker_arch_alt-ergo-2.3.3_1"; opts = [ "ls"; "-a" ] };
    { cmd = "docker_arch_alt-ergo-2.3.2_1"; opts = [ "ls"; "-a" ] };
    { cmd = "docker_arch_alt-ergo-2.3.0_1"; opts = [ "ls"; "-a" ] };
    { cmd = "docker_arch_alt-ergo-2.3.1_1"; opts = [ "ls"; "-a" ] };
    { cmd = "docker_arch_alt-ergo-2.4.1_1"; opts = [ "ls"; "-a" ] };
    { cmd = "docker_arch_alt-ergo-2.4.0_1"; opts = [ "ls"; "-a" ] };
  ]

let timelimit_opt (solver : string) (tl : int) =
  if solver = "cvc" then "--tlimit=" ^ string_of_int (tl * 1000)
  else if solver = "z3" then "-T:" ^ string_of_int tl
  else "-t " ^ string_of_int tl

(** Builds commands that will be given to [docker exec] *)
let cmds_builder (toml_ht : (string, string) Stdlib__hashtbl.t)
    (files_l : string list) (max_containers_available : int ref) =
  let all_cmds = [] in
  let solver = Hashtbl.find toml_ht "jd_solver" in
  let solver_version = Hashtbl.find toml_ht "jd_solver_version" in
  let verbosity = bool_of_string (Hashtbl.find toml_ht "jd_verbosity") in
  let time_limit = int_of_string (Hashtbl.find toml_ht "jd_time_limit") in
  let stats = bool_of_string (Hashtbl.find toml_ht "jd_stats") in
  List.fold_right
    (fun x l_acc ->
      {
        cmd =
          "docker_arch_" ^ solver ^ "-" ^ solver_version ^ "_"
          ^ string_of_int (Random.int !max_containers_available + 1);
        opts =
          [
            (if solver = "cvc" || solver = "alt-ergo" then
             solver ^ "-" ^ solver_version
            else solver);
            (if verbosity && solver <> "z3" then "-v" else "-v:10");
            (if stats && solver = "cvc" then "--stats" else if solver = "z3" then "-st" else "");
            timelimit_opt solver time_limit;
            x;
          ];
      }
      :: l_acc)
    files_l all_cmds

let run_cmd (cmds : opt_l) =
  print_endline (Printf.sprintf "Executing : %s" (opt_l_tostring cmds));
  let dlc = C.list ~all:false ~size:true () in
  let c_names = Misc.list_range 1 !nb_duplicates in
  let targets =
    List.fold_left
      (fun res (x : int) ->
        T.ids_from_containers_list_wname
          ("/"
          ^ String.sub cmds.cmd 0 (String.length cmds.cmd - 1)
          ^ string_of_int x)
          dlc
        :: res)
      [] c_names
  in
  let available_targets =
    List.filter (fun x -> List.length (C.l_procs x) = 1) targets
  in
  (* print_endline
     (Printf.sprintf "targets : %s"
        (stringlist_tostring " // " available_targets)); *)
  while List.length available_targets = 0 do
    print_endline "sleeping : no free containers available";
    Unix.sleepf 0.5
  done;
  let e =
    C.Exec.create
      (List.nth available_targets (Random.int @@ List.length available_targets))
      cmds.opts
  in
  let st = C.Exec.start e in
  let s = Docker.Stream.read_all st in
  let identify (ty, s) =
    match ty with
    | Docker.Stream.Stdout -> "out> " ^ s
    | Docker.Stream.Stderr -> "err> " ^ s
  in
  let path_res =
    List.hd @@ String.split_on_char '/' (List.hd @@ List.rev cmds.opts)
  in
  let oc =
    Stdlib.open_out
      (storage_dir ^ "/" ^ path_res ^ "/results/" ^ cmds.cmd ^ "-"
      ^ (Filename.basename @@ List.nth (List.rev cmds.opts) 0)
      ^ ".txt")
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

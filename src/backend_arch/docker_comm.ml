open Printf
open Docker

module C = Docker.Container
module T = Docker.Tools

let () =
  Printexc.record_backtrace true;
  (* Common.install_image "debian" ~tag:"latest"; *)
  let dlc = C.list ~all:false ~size:true () in
  List.iter
    (fun x -> print_endline @@ sprintf "%s" (T.container_info_to_string x))
    dlc;
  print_endline
  @@ sprintf "Printing ids of running container : \n%s"
       (T.stringlist_tostring " :: \n"
          (T.flatten_l @@ T.names_from_containers_list dlc));
  print_endline
  @@ sprintf "Printing ids of running container : \n%s"
       (T.stringlist_tostring " :: \n" (T.ids_from_containers_list dlc));

  (* let c = C.create "debian:latest" [ "bash"; "-s" ] ~open_stdin:true in
     C.start c; *)
  print_endline
    (T.ids_from_containers_list_wname "/sad_lehmann" dlc);
  let e =
    C.Exec.create
      (T.ids_from_containers_list_wname "/sad_lehmann" dlc)
      [ "alt-ergo-2.4.1"; "-vp"; "ALIA/piVC/piVC_030ee9.smt2" ]
  in
  let st = C.Exec.start e in
  (* fprintf (Docker.Stream.out st) "ls -l /home/\n%!"; *)
  let s = Docker.Stream.read_all st in
  (* Docker.Container.stop c;
     Docker.Container.rm c; *)
  let identify (ty, s) =
    match ty with
    | Docker.Stream.Stdout -> "out> " ^ s
    | Docker.Stream.Stderr -> "err> " ^ s
  in
  printf "Exec in the container returned:\n%s\n"
    (String.concat "\n" (List.map identify s))
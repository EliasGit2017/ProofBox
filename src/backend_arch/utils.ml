open Otoml
open Str

(* Otoml : Utils to get / set values and acces toml files simply &
   Wrappers over some functions *)

let owner_username = [ "owner"; "username" ]
let owner_email = [ "owner"; "email" ]
let owner_bio = [ "owner"; "bio" ]
let owner_password = [ "owner"; "password" ]
let job_description_id = [ "job_description"; "job_id" ]
let job_description_solver = [ "job_description"; "solver" ]
let job_description_solver_version = [ "job_description"; "solver_version" ]
let job_description_synopsis = [ "job_description"; "synopsis" ]
let job_description_path_tof = [ "job_description"; "path_to_client_repo" ]

(** Bad list to str *)
let stringl_to_str s_l = List.fold_left ( ^ ) "" s_l

(** change Otoml.TomlBoolean to bool (decaps/encaps) *)
let get_value_wrap parsed_toml path_toval =
  try Otoml.find parsed_toml Otoml.get_value path_toval with
  | Type_error e ->
      print_endline
      @@ Printf.sprintf "Otoml.Type_error from get_value_wrap : %s" e;
      Otoml.TomlBoolean false
  | Key_error e ->
      print_endline
      @@ Printf.sprintf "Otoml.Type_error from get_value_wrap : %s" e;
      Otoml.TomlBoolean false

let get_str parsed_toml path_toval =
  Otoml.find parsed_toml (Otoml.get_string ~strict:true) path_toval

let get_int parsed_toml path_toval =
  Otoml.find parsed_toml (Otoml.get_integer ~strict:true) path_toval

let get_owner_username parsed_toml = get_value_wrap parsed_toml owner_username
let get_owner_bio parsed_toml = get_value_wrap parsed_toml owner_bio
let get_owner_bio parsed_toml = get_value_wrap parsed_toml owner_bio
let get_owner_password parsed_toml = get_value_wrap parsed_toml owner_password

let get_job_description_job_id parsed_toml =
  get_value_wrap parsed_toml job_description_id

let get_job_description_solver parsed_toml =
  get_value_wrap parsed_toml job_description_solver

let get_job_description_solver_version parsed_toml =
  get_value_wrap parsed_toml job_description_solver_version

let get_job_description_job_synopsis parsed_toml =
  get_value_wrap parsed_toml job_description_synopsis

let get_job_description_path_tof parsed_toml =
  get_value_wrap parsed_toml job_description_path_tof

(* Unix : tools *)

let path_to_toml = "/home/elias/OCP/ez_proofbox/src/backend_arch"

let testunix = Unix.getcwd

let accepted_formats = [ ".smt2"; ".ae" ]

let stat_code status =
  match status with
  | Unix.WEXITED e -> Printf.sprintf "WEXITED : code = %d" e
  | Unix.WSIGNALED s -> Printf.sprintf "WSIGNALED : code = %d" s
  | Unix.WSTOPPED st -> Printf.sprintf "WSTOPPED : code = %d" st

(** Print channel with [print_endline]  *)
let print_chan channel =
  let rec loop () =
    let () = print_endline (input_line channel) in
    loop ()
  in
  try loop () with End_of_file -> close_in channel

(** Search for no ref version ? *)
let chan_to_stringlist channel =
  let l_res = ref List.[] in
  let rec loop () =
    l_res := List.append !l_res [ input_line channel ];
    loop ()
  in
  try loop ()
  with End_of_file ->
    (* close_in channel; *)
    (* Closed by [Unix.close_process_full] *)
    !l_res

(** Print string list with [print_endline] for each elem *)
let rec stringlist_printer = function
  | [] -> ()
  | e :: l ->
      print_endline e;
      stringlist_printer l

(** Convert string list to string with [sep] as separator *)
let rec sringlist_tostring sep = function
  | [] -> ""
  | "" :: l -> sringlist_tostring sep l
  | e :: l -> e ^ sep ^ sringlist_tostring sep l

(* Alternatives *)
(* let join l = List.filter (fun s -> s <> "") l |> String.concat "" *)
(* let join2 sep l = String.concat sep l *)

let get_all_files_w_ext wd =
  Sys.readdir wd |> Array.to_list
  |> List.filter (fun x ->
         Filename.extension x = ".smt2" || Filename.extension x = ".ae")

(** [dir_contents] returns the paths of all regular files ([.smt2] && [.ae]) that are
  contained in [dir]. Each file is a path starting with [dir].*)
let dir_contents dir =
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
        Sys.readdir f |> Array.to_list
        (* |> List.filter (fun x ->
               let x_bis = List.hd (List.rev (String.split_on_char '/' x)) in
               print_endline x_bis;
               Filename.extension x_bis = ".smt2") *)
        (* |> List.filter (fun x -> Str.string_match (Str.regexp {|.*\.smt2|}) x 0) *)
        (* |> List.filter (fun x -> Filename.extension x = ".smt2") *)
        |> List.map (Filename.concat f)
        |> List.append fs |> loop result
    | f :: fs -> loop (f :: result) fs
    | [] ->
        result
        |> List.filter (fun x ->
               Str.string_match (Str.regexp {|\(.*\.smt2\)\|(.*\.ae)|}) x 0)
    (* not optimized *)
  in
  loop [] [ dir ]

(** Exploration && tests *)
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

open Zip


(** deflate zip to result directory  : TOOPT *)
let deflate_zip_archive zip_arch_name dest =
  let dest_dir = dest ^ "/results/" in
  let _ = Sys.command ("mkdir -p " ^ dest_dir) in
  let in_f = open_in zip_arch_name in
  let arch_entries = entries in_f in
  List.iter
    (fun x -> copy_entry_to_file in_f x (dest_dir ^ x.filename))
    arch_entries


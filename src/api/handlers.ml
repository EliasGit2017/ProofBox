open Lwt.Infix
open Data_types
(* open Services *)
open Db

let to_api_v0 p = Lwt.bind p EzAPIServerUtils.return
(** Initial to_api  *)

let to_api p =
    Lwt.catch
        (fun () -> Lwt.bind p EzAPIServerUtils.return)
        (fun err ->
            match err with
            | Proofbox_api_error exn -> EzAPIServerUtils.return ~code:500 (Error exn)
            | _ -> EzAPIServerUtils.return ~code:500 (Error Unknown))
(** [to_api p] executes [p] asynchroniously and encapsulates promise value to answer [EzAPIServerUtils.Answer.t].
    Catches and encapsulates server errors raised by [p] in order to get error's constructors
    [Data_types.server_error_type] from client-side. If [p] raises another type of error, then it is
    converted to [Unknown].*)

let version _params () = to_api (
    (* let () = bod_ver in  *)
    Db.get_version () >|= fun v_db_version ->
        Ok { v_db = PConfig.database; v_db_version })

let version_test_json_body _params elem = to_api (
    print_endline elem.basic;
    Db.get_version () >|= fun v_db_version ->
        Ok { v_db = PConfig.database; v_db_version })

let sr_job_desc _params elem = to_api (
    (* print_endline @@ elem.job_client_r ^ elem.job_ref_tag_v; *)
    Db.get_job_desc elem >|= fun jobs ->
        Ok jobs)

let get_all_jobs _params req = to_api (
    Db.get_all_jobs_from_user req >|= fun jobs ->
        Ok jobs
)
open Js_of_ocaml
open Js
open Data_types
(* open EzAPI.TYPES *)


let get_app ?app () = match app with None -> V.app () | Some app -> app

let route ?app path =
  Common.logs ("route " ^ path);
  let app = get_app ?app () in
  app##.path := string path;
  match String.split_on_char '/' path with
  | [ path ] -> (
      match path with
      | "db" ->
          Request.get0 Services.version (fun { v_db; v_db_version } ->
              app##.database := string v_db;
              app##.db_version_ := v_db_version)
          (* (* let api = Common.test_host_api in *)
             EzRequest.ANY.post0 ~msg:"Service.version_test_json_body" Common.test_host_api
             Services.version_test_json_body
             ~error:(failwith "Bad test in route.ml")
             ~input: {basic = "this is a test"}
             (function
               | Ok r ->
                 app##.database := string r.v_db;
                 app##.db_version_ := r.v_db_version
               | Error _ ->
                 Js_of_ocaml.Firebug.console##log (Js.string "Bad request JS!")) *)
      | "api" ->
          Common.wait ~t:10. @@ fun () ->
          (Unsafe.pure_js_expr "Redoc")##init
            (string "openapi.json")
            (Unsafe.obj [| ("scrollYOffset", Unsafe.inject 50) |])
            (Dom_html.getElementById_exn "redoc")
      | _ -> ())
  | _ -> ()

let route_js app path =
  route ~app (to_string path);
  Common.set_path (to_string path)

let init () =
  V.add_method1 "route" route_js;
  let path = Common.path () in
  Dom_html.window##.onpopstate
  := Dom_html.handler (fun _e ->
         route @@ Common.path ();
         _true);
  path


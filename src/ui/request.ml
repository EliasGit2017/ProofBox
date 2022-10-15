open Data_types
(* module S = Services *)

let host = ref (EzAPI.TYPES.BASE PConfig.web_host)

let server_errors_handler err =
  match err with
  | Invalid_request -> Common.warn "Server_error_type : Invalid_request"
  | No_sources_config -> Common.warn "Server_error_type : Invalid_argument"
  | Unknown -> Common.warn "Server_error_type : Unknown"

let wrap_res ?error f = function
  | Ok x -> f x
  | Error exn -> server_errors_handler exn

let get0 ?post ?headers ?params ?error ?msg ?(host = !host) service f =
  EzRequest.ANY.get0 host service ?msg ?post ?headers ?error ?params
    (wrap_res ?error f)

let get1 ?post ?headers ?params ?error ?msg ?(host = !host) service f arg =
  EzRequest.ANY.get1 host service ?msg ?post ?headers ?error ?params
    (wrap_res ?error f) arg

let info_service :
    (www_server_info, server_error_type, EzAPI.no_security) EzAPI.service0 =
  EzAPI.service ~output:Encoding.info_encoding EzAPI.Path.(root // "info.json")

let init f =
  get0 ~host:(Common.host ()) info_service
    ~error:(fun code content ->
      let s =
        match content with
        | None -> "network error"
        | Some content ->
            "network error: " ^ string_of_int code ^ " -> " ^ content
      in
      Common.logs s)
    (fun ({ www_apis; _ } as info) ->
      let api = List.nth www_apis (Random.int @@ List.length www_apis) in
      host := EzAPI.TYPES.BASE api;
      f info)

open Js_of_ocaml
open EzAPI.TYPES

let test_host_api = BASE (PConfig.api_host ^ (string_of_int PConfig.api_port))

let html_escaped s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len -1 do
    match s.[i] with
    | '<' -> Buffer.add_string b "&lt;"
    | '>' -> Buffer.add_string b "&gt;"
    | '&' -> Buffer.add_string b "&amp;"
    | '"' -> Buffer.add_string b "&quot;"
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b

let js s = Js.string s
(** Converts [string] to [js_string t].*)

let host () =
  let host =
    match Url.url_of_string (Js.to_string Dom_html.window##.location##.href) with
    | Some (Url.Http hu) -> Misc.spf "http://%s:%d" hu.Url.hu_host hu.Url.hu_port
    | Some (Url.Https hu) -> Misc.spf "https://%s:%d" hu.Url.hu_host hu.Url.hu_port
    | _ -> PConfig.web_host in
  EzAPI.TYPES.BASE host

let logs s = Firebug.console##log (js s)

let path () =
  match Url.url_of_string (Js.to_string Dom_html.window##.location##.href) with
  | None -> ""
  | Some url -> match url with
    | Url.Http hu | Url.Https hu -> String.concat "/" hu.Url.hu_path
    | Url.File fu -> String.concat "/" fu.Url.fu_path

let set_path ?(scroll=true) ?(args=[]) path =
  let args = match args with
    | [] -> ""
    | l -> "?" ^ String.concat "&" (List.map (fun (k, v) -> k ^ "=" ^ v) l) in
  let path = Js.some @@ Js.string @@ "/" ^ path ^ args in
  Dom_html.window##.history##pushState path (Js.string "") path;
  if scroll then Dom_html.window##scroll 0 0

let wait ?(t=1.) f =
  Dom_html.window##setTimeout (Js.wrap_callback f) t |> ignore

let warn s = Firebug.console##warn (js s)
(** [warn s] prints [s] in console as a warning. *)
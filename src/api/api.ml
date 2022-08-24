open EzAPIServerUtils

module MakeRegisterer(S: module type of Services)(H:module type of Handlers) = struct

  let register s h dir =
    let h a _ b = h a b in
    register s h dir

  let register dir =
    dir
  |> register S.version H.version
  |> register S.version_test_json_body H.version_test_json_body
  |> register S.sr_job_desc H.sr_job_desc
  |> register S.sr_job_desc_from_user H.get_all_jobs
  |> register S.test_session H.test_session
  |> register S.sign_up_new_user H.sign_up_new_user
  |> register S.send_job_metadata H.job_metadata
  |> register_ws S.zip_tranfer ~react:H.react_server_zip_ws0 ~bg:H.background_zip_ws0
  (* |> register_ws S.zip_tranfer ~react:H.zip_react ~bg:H.zip_bg *)
  
  |> register_ws S.service ~react:H.react ~bg:H.bg
  |> Handlers.My_Session.register_handlers

end

module R = MakeRegisterer(Services)(Handlers)

let services =
  empty |> R.register
  

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

end

module R = MakeRegisterer(Services)(Handlers)

let services =
  empty |> R.register

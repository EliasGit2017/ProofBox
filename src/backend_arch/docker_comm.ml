open Docker
open Dockerfile
open Dockerfile_cmd

let () =
  let _ = Docker.push_cmd "echo \"test\"" in ()
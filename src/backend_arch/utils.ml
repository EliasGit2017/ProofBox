open Toml

let force_opt opt =
  match opt with
  | Some value -> value
  | None -> failwith "No value"

let get_string k toml_table =
  Toml.Lenses.(get toml_table (key k |-- string)) |> force_opt

let get_int k toml_table =
  Toml.Lenses.(get toml_table (key k |-- int)) |> force_opt

let get_float k toml_table =
  Toml.Lenses.(get toml_table (key k |-- float)) |> force_opt

let get_bool k toml_table =
  Toml.Lenses.(get toml_table (key k |-- bool)) |> force_opt

let get_bool_array k toml_table =
  Toml.Lenses.(get toml_table (key k |-- array |-- bools)) |> force_opt

let get_table k toml_table =
  Toml.Lenses.(get toml_table (key k |-- table)) |> force_opt

let get_table_array k toml_table =
  Toml.Lenses.(get toml_table (key k |-- array |-- tables)) |> force_opt

let unsafe_from_string s = Toml.Parser.from_string s |> Toml.Parser.unsafe
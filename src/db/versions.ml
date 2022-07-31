

let counter_ver = ref 0

let upgrades : (int * (unit PGOCaml.t -> int -> unit)) list ref = ref []

let downgrades : (int * string list) list ref = ref []

let register_version ?version ?(before_upgrade = fun _ -> ())
  ?(after_upgrade = fun _ -> ())
  ~downgrade
  ~upgrade () =
  let prev_version = !counter_ver in
  let version = match version with
  | None -> !counter_ver + 1
  | Some v -> 
    if v <= !counter_ver then
      Format.ksprintf failwith "Registering version %d forbidden (min %d)"
      v (!counter_ver + 1);
    v in
  let upgrade dbh version =
    before_upgrade dbh;
    EzPG.upgrade ~dbh ~version ~downgrade upgrade;
    after_upgrade dbh;
  in
  counter_ver := version;
  upgrades := !upgrades @ [prev_version, upgrade];
  downgrades := (version, downgrade) :: !downgrades;
  ()
;;


let init () =
  register_version ()
  ~upgrade:[
    {| CREATE EXTENSION if not exists "uuid-ossp";|};
       {|CREATE EXTENSION if not exists citext;|};
       {| DO $$
          BEGIN
          IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'domain_email') THEN
                  CREATE DOMAIN domain_email AS citext
                  CHECK(
                    VALUE ~ '^([a-zA-Z0-9_\-\.]+)@([a-zA-Z0-9_\-\.]+)\.([a-zA-Z]{2,5})$'
                  );
          END IF;
        END$$;
    |};
    {| CREATE TABLE jobs_description
    (
       job_id SERIAL PRIMARY KEY,
       job_client VARCHAR NOT NULL,
       order_ts VARCHAR NOT NULL,
       path_to_f VARCHAR NOT NULL,
       status VARCHAR NOT NULL
    )
    |};
    {| CREATE TABLE jobs_cache
    (
       job_id SERIAL PRIMARY KEY,
       path_to_results VARCHAR NOT NULL UNIQUE,
       time_taken VARCHAR NOT NULL,
       status VARCHAR NOT NULL
    )
    |};
    {| CREATE TABLE users
    (
       username VARCHAR PRIMARY KEY,
       email domain_email NOT NULL UNIQUE,
       password VARCHAR NOT NULL UNIQUE,
       user_desc TEXT NOT NULL,
       first_login_date VARCHAR NOT NULL
    )
    |};
    (* Dummy values to populate db for testing purposes *)
    {|INSERT INTO users (username, email, password, user_desc, first_login_date)
      values ('ocamlpro', 'azwbdj@gmail.com', 'this_will_be_hashed', 'Real OG, first proofbox user', '29-07-2022-12-00-00'); |};
    {|INSERT INTO jobs_description (job_client, order_ts, path_to_f, status) 
      values ('ocamlpro', '29-07-2022-17-45-30', 'root', 'scheduled'); |};
    {|INSERT INTO jobs_description (job_client, order_ts, path_to_f, status)
      values ('ocamlpro', '31-07-2022-18-15-30', 'root', 'scheduled'); |};
    {|INSERT INTO jobs_description (job_client, order_ts, path_to_f, status) 
      values ('ocamlpro', '29-07-2022-17-45-30', 'root', 'scheduled'); |}
  ]
  ~downgrade:[
    {|DROP TABLE jobs_description CASCADE|};
    {|DROP TABLE jobs_cache CASCADE|};
    {|DROP TABLE users CASCADE|};
    {|DROP EXTENSION "uuid-ossp"|};
    {|DROP DOMAIN domain_email|};
    {|DROP EXTENSION citext|}
  ]
;;

let () = init ()

(* Skeleton predefined variables and function *)

(* let downgrade_1_to_0 = []

let upgrade_0_to_1 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_1_to_0 []

let upgrades = [
  0, upgrade_0_to_1
]

let downgrades = [
  1, downgrade_1_to_0
] *)


open Data_types
open Bcrypt
open Db

let load_testing_users =
  let user1 =
    {
      username = "james";
      email = "james.dean@gmail.com";
      password = "examPlePass1!";
      user_desc = "test user 1";
      first_login_date = "2022-08-07 14:45:52.523274";
    }
  in
  let user2 =
    {
      username = "marla";
      email = "marla1991@hotmail.fr";
      password = "Rule1$!%";
      user_desc = "Here for testing only";
      first_login_date = "2022-08-07 14:45:52.523274";
    }
  in
  let user3 =
    {
      username = "tyler";
      email = "tyler_durden@gmail.com";
      password = "2ndRulerefRule1$!";
      user_desc = "3rd user for testing only";
      first_login_date = "2022-08-07 14:45:52.523274";
    }
  in
  let user_list = [ user1; user2; user3 ] in
  let hashed_password_list =
    List.map
      (fun e ->
        {
          username = e.username;
          email = e.email;
          password = Handlers.hash_bcrypt e.password;
          user_desc = e.user_desc;
          first_login_date = e.first_login_date;
        })
      user_list
  in
  Lwt_list.map_s Db.add_user_to_db hashed_password_list


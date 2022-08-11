open Data_types
open Bcrypt
open Db

let user_test1 =
  {
    username = "test_user0";
    email = "azwbdklo@gmail.com";
    password = "dummy1234!";
    user_desc = "This user is here for testing purposes";
    first_login_date = "25-04-1997 20:45:30";
  }

let user_test2 =
  {
    username = "test_user1";
    email = "azwbdj@hotmail.com";
    password = "dummy1234!";
    user_desc = "This user is also here for testing purposes";
    first_login_date = "25-04-1997 20:45:30";
  }

let user_test3 =
  {
    username = "test_user60";
    email = "azwbdjefegdsdaqdzar@gmail.com";
    password = "dummydedada1234!";
    user_desc = "This user is also here for testing purposes";
    first_login_date = "25-04-1997 20:45:30";
  }

let user_test_tyler =
  {
    username = "tyler";
    email = "tyler_durden@gmail.com";
    password = "2ndRulerefRule1$!";
    user_desc = "3rd user for testing only";
    first_login_date = "2022-08-07 14:45:52.523274";
  }

let user_test_marla =
  {
    username = "marla";
    email = "marla1991@hotmail.fr";
    password = "Rule1$!%";
    user_desc = "Here for testing only";
    first_login_date = "2022-08-07 14:45:52.523274";
  }

let user_test_james =
  {
    username = "james";
    email = "james.dean@gmail.com";
    password = "examPlePass1!";
    user_desc = "test user 1";
    first_login_date = "2022-08-07 14:45:52.523274";
  }



let default_users_list = [user_test1; user_test2; user_test3]

let load_testing_users =
  (* let user_list = [ user1; user2; user3 ] in *) (* custom list of users *)
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
      default_users_list
  in
  Lwt_list.map_s Db.add_user_to_db hashed_password_list


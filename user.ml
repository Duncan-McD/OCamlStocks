type t = {
  email : string;
  name : string;
  password : string;
  current_portfolio : Portfolio.t;
  past_portfolios : Portfolio.t list;
  test_portfolios : Portfolio.t list;
  config : Config.t;
  account_creation_time : float;
  last_daily_task_timestamp : float;
}

let create email name password =
  {
    email;
    name;
    password;
    current_portfolio = Portfolio.empty_portfolio;
    past_portfolios = [];
    test_portfolios = [];
    config = Config.default;
    account_creation_time = Unix.time ();
    last_daily_task_timestamp = 86400.;
  }

let email user = user.email

let name user = user.name

let last_name user =
  let index_of_space = String.index_opt user.name ' ' in
  match index_of_space with
  | None -> user.name
  | Some i -> String.sub user.name i (String.length user.name - i)

let first_name user =
  let index_of_space = String.index_opt user.name ' ' in
  match index_of_space with
  | None -> user.name
  | Some i -> String.sub user.name 0 i

let password user = user.password

let current_portfolio user = user.current_portfolio

let past_portfolios user = user.past_portfolios

let test_portfolios user = user.test_portfolios

let last_daily_task_timestamp user = user.last_daily_task_timestamp

let account_creation_time user = user.account_creation_time

let config user = user.config

let change_config user configuration =
  {
    email = email user;
    name = name user;
    password = password user;
    current_portfolio = current_portfolio user;
    past_portfolios = user.past_portfolios;
    test_portfolios = user.test_portfolios;
    config = configuration;
    account_creation_time = user.account_creation_time;
    last_daily_task_timestamp = last_daily_task_timestamp user;
  }

let update_portfolio user portfolio =
  {
    email = email user;
    name = name user;
    password = password user;
    current_portfolio = portfolio;
    past_portfolios = user.current_portfolio :: user.past_portfolios;
    test_portfolios = user.test_portfolios;
    config = user.config;
    account_creation_time = user.account_creation_time;
    last_daily_task_timestamp = last_daily_task_timestamp user;
  }

let change_test_portfolios user portfolios =
  {
    email = email user;
    name = name user;
    password = password user;
    current_portfolio = current_portfolio user;
    past_portfolios = past_portfolios user;
    test_portfolios = portfolios;
    config = config user;
    account_creation_time = user.account_creation_time;
    last_daily_task_timestamp = last_daily_task_timestamp user;
  }

let set_last_daily_task_timestamp user timestamp =
  {
    email = email user;
    name = name user;
    password = password user;
    current_portfolio = current_portfolio user;
    past_portfolios = past_portfolios user;
    test_portfolios = test_portfolios user;
    config = config user;
    account_creation_time = user.account_creation_time;
    last_daily_task_timestamp = timestamp;
  }

open Yojson.Basic.Util

let rec portfolio_list_to_json (portfolio_list : Portfolio.t list) acc =
  match portfolio_list with
  | [] -> `List acc
  | h :: t -> portfolio_list_to_json t (Portfolio.to_json h :: acc)

let to_json t =
  `Assoc
    [
      ("email", `String t.email);
      ("name", `String t.name);
      ("password", `String t.password);
      ("current_portfolio", Portfolio.to_json t.current_portfolio);
      ("past_portfolios", portfolio_list_to_json t.past_portfolios []);
      ("test_portfolios", portfolio_list_to_json t.test_portfolios []);
      ("config", Config.to_json t.config);
      ("account_creation_time", `Float t.account_creation_time);
      ("last_daily_task_timestamp", `Float t.last_daily_task_timestamp);
    ]

let rec portfolio_list_of_json (j : Yojson.Basic.t list) acc =
  match j with
  | [] -> List.rev acc
  | h :: t -> portfolio_list_of_json t (Portfolio.portfolio_of_json h :: acc)

let user_of_json (j : Yojson.Basic.t) =
  {
    email = to_string (member "email" j);
    name = to_string (member "name" j);
    password = to_string (member "password" j);
    current_portfolio =
      Portfolio.portfolio_of_json (member "current_portfolio" j);
    past_portfolios =
      portfolio_list_of_json (to_list (member "past_portfolios" j)) [];
    test_portfolios =
      portfolio_list_of_json (to_list (member "test_portfolios" j)) [];
    config = Config.config_of_json (member "config" j);
    account_creation_time = to_float (member "account_creation_time" j);
    last_daily_task_timestamp = to_float (member "last_daily_task_timestamp" j);
  }

let time_for_daily_tasks user timestamp =
  timestamp -. last_daily_task_timestamp user >= 86400.

let set_email user email_input =
  {
    email = email_input;
    name = name user;
    password = password user;
    current_portfolio = current_portfolio user;
    past_portfolios = past_portfolios user;
    test_portfolios = test_portfolios user;
    config = config user;
    account_creation_time = user.account_creation_time;
    last_daily_task_timestamp = user.last_daily_task_timestamp;
  }

let set_username user username_input =
  {
    email = email user;
    name = username_input;
    password = password user;
    current_portfolio = current_portfolio user;
    past_portfolios = past_portfolios user;
    test_portfolios = test_portfolios user;
    config = config user;
    account_creation_time = user.account_creation_time;
    last_daily_task_timestamp = user.last_daily_task_timestamp;
  }

let set_password user password_input =
  {
    email = email user;
    name = name user;
    password = password_input;
    current_portfolio = current_portfolio user;
    past_portfolios = past_portfolios user;
    test_portfolios = test_portfolios user;
    config = config user;
    account_creation_time = user.account_creation_time;
    last_daily_task_timestamp = user.last_daily_task_timestamp;
  }

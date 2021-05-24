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
    last_daily_task_timestamp = 0.0;
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

let time_for_daily_tasks user timestamp =
  timestamp -. last_daily_task_timestamp user >= 86400.

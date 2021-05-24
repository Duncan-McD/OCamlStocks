type t = {
  email : string;
  name : string;
  password : string;
  current_portfolio : Portfolio.t;
  past_portfolios : Portfolio.t list;
  test_portfolios : Portfolio.t list;
  config : Config.t;
}

let create email name password =
  {
    email;
    name;
    password;
    current_portfolio = Portfolio.empty_portfolio;
    past_portfolios = [];
    test_portfolios = [];
    config = Config.default ();
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
  }

open Yojson.Basic.Util

let rec string_of_porfolio_list portfolio_list acc =
  match portfolio_list with
  | [] -> acc
  | h :: t -> string_of_porfolio_list t (Portfolio.to_json_string h ^ "," ^ acc)

let to_json_string t =
  "{" ^ "\"email: \"" ^ t.email ^ ", " ^ "\"name: \"" ^ t.name ^ ", "
  ^ "\"password: \"" ^ t.password ^ "," ^ "\"current_portfolio: \""
  ^ Portfolio.to_json_string t.current_portfolio
  ^ "," ^ "\"past_portfolios: \"" ^ "["
  ^ string_of_porfolio_list t.past_portfolios ""
  ^ "]" ^ ", " ^ "," ^ "\"test_portfolios: \"" ^ "["
  ^ string_of_porfolio_list t.test_portfolios ""
  ^ "]" ^ ", " ^ "\"config: \""
  ^ Config.to_json_string t.config
  ^ "}"

let rec portfolio_list_of_json (j : Yojson.Basic.t list) acc =
  match j with
  | [] -> acc
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
  }

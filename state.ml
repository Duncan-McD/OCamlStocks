type action =
  | Help
  | Menu
  | Configure
  | Run_Algoirthm
  | Sell_All
  | Refresh_and_Show
  | Graph
  | Account
  | Logout
  | Quit

type t = { mutable config : Config.t; mutable portfolio : Portfolio.t }

exception InvalidAction of string

exception QuitAction

exception LogoutAction

exception MenuAction

let init = { config = Config.default (); portfolio = Portfolio.empty_portfolio }

let action_of_string s =
  let s' = String.lowercase_ascii s in
  if s' = "menu" then Menu
  else if s' = "help" then Help
  else if s' = "quit" then Quit
  else if s' = "logout" then Logout
  else if s' = "configure" then Configure
  else if s' = "run" then Run_Algoirthm
  else if s' = "sell all" then Sell_All
  else if s' = "show data" then Refresh_and_Show
  else if s' = "graph data" then Graph
  else raise (InvalidAction s)

let update state = function
  | Menu -> raise MenuAction
  | Quit -> raise QuitAction
  | Logout -> raise LogoutAction
  | _ -> ()

let config state = state.config

let portfolio state = state.portfolio

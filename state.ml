type action =
  | Help
  | Menu
  | Configure
  | Run_Algorithm
  | Sell_All
  | Refresh_and_Show
  | Graph
  | Account
  | Logout
  | Quit

type t = { auth : Auth.auth; mutable user : User.t }

exception InvalidAction of string

exception QuitAction

exception LogoutAction

exception MenuAction

let init auth user = { auth; user }

let action_of_string s =
  let s' = String.lowercase_ascii s in
  if s' = "menu" then Menu
  else if s' = "help" then Help
  else if s' = "quit" then Quit
  else if s' = "logout" then Logout
  else if s' = "configure" then Configure
  else if s' = "run" then Run_Algorithm
  else if s' = "sell all" then Sell_All
  else if s' = "show data" then Refresh_and_Show
  else if s' = "graph data" then Graph
  else raise (InvalidAction s)

let menu ?(initial = false) state =
  if initial = false then
    ANSITerminal.print_string [ ANSITerminal.magenta ]
      "\nOStocker Actions Menu:\n\n"
  else if state.auth = Login then
    ANSITerminal.print_string [ ANSITerminal.magenta ]
      ( "\nWelcome back " ^ User.name state.user
      ^ "! In case you forgot, here are your options:\n\n" )
  else
    ANSITerminal.print_string [ ANSITerminal.magenta ]
      ( "\nWelcome to " ^ User.name state.user
      ^ "Below you can see all of the options available to you.\n\n" );

  print_endline
    "\"help\" : walk you through how I works and how to use me\n\
     \"configure\" : configure my subreddit and optimization settings\n\
     \"run\" : run my algorithm and buy / sell stocks accordingly \n\
     \"sell all\" : sell all of your owned stocks\n\
     \"show data\" : show text data about your portfolio\n\
     \"graph data\" : graph data about your portfolio\n\
     \"menu\" : show this menu\n\
     \"logout\" : logout\n\
     \"quit\" : quit\n";
  ()

let update state = function
  | Menu -> menu state
  | Quit -> raise QuitAction
  | Help -> failwith ""
  | Logout -> raise LogoutAction
  | Run_Algorithm -> ()
  (* time_for_daily_tasks will return true if call optizimer function or not in alg *)
  | Sell_All -> () (* call set portfolio to result of portfolio.sell_all *)
  | _ -> ()

let user state = state.user

let config state = User.config state.user

let portfolio state = User.current_portfolio state.user

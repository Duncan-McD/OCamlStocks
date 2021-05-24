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

exception HelpAction

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

let help () =
  let border = "=====================" in
  print_endline border;
  print_string
    "Welcome to the Help Page! Here is a little tutorial of how to use this \
     program. (press ENTER to continue or type anything and ENTER to quit)";
  let help_instructions =
    [
      {|"configure" allows you to customize your settings like the subreddits you would like to reference and the use of optional optimization features; you can also add money to be used by the program here|};
      {|"optimize" prompts the program to save relevant data needed to optimize its algorithm and also alerts you of the time elapsed since last optimizing (should be run before "run")|};
      {|"run" is the main function which runs algorithm (according to your configurations) and buys or sells stocks according to the data found at run time|};
      {|"sell all" allows you to sell all of your currently owned stocks|};
      {|"show data" shows updated data about the stocks you currently own|};
      {|"graph data" generates a graph about the stocks you currently own|};
      {|"menu" shows a simple menu with all the possible commands|};
      {|"logout" logs you out of your session in the program|};
      {|"quit" simply terminates the program and exits|};
      {|Now that you know all of my actions, let's run through an example workflow a user might go through! (press ENTER to continue or type anything and ENTER to quit)|};
      {|First, you want to check that your configurations are what you would like them to be, although the provided defaults are more than sufficient to start (no money needs to be added just yet)|};
      {|Next, run "optimize" to start the optimization process. It is recommended that you wait around a day before beginning to run the program in order to buy or sell any stocks using "run".|};
      {|Keep in mind that there is no need to keep the program running while waiting; feel free to start the program up again once some time has passed.|};
      {|Then, run "optimize" once more before running "run" so that the algorithm will be ran using the optimization.|};
      {|Finally, repeat waiting then running "optimize" and "run" for as long as desired and "sell all" when done; making sure to check your stocks along the way.|};
    ]
  in
  let pause_escape str =
    match read_line () with
    | exception End_of_file -> ()
    | "" -> print_string str
    | _ ->
        print_endline @@ border ^ "\n";
        raise (Failure "quit help")
  in
  try
    List.iter pause_escape help_instructions;
    print_endline @@ "\n" ^ border ^ "\n"
  with
  | Failure _ -> ()
  | _ -> ()

let update state = function
  | Menu -> menu state
  | Help -> help ()
  | Quit -> raise QuitAction
  | Logout -> raise LogoutAction
  | Run_Algorithm -> ()
  (* time_for_daily_tasks will return true if call optizimer function or not in alg *)
  | Sell_All -> () (* call set portfolio to result of portfolio.sell_all *)
  | _ -> ()

let user state = state.user

let config state = User.config state.user

let portfolio state = User.current_portfolio state.user

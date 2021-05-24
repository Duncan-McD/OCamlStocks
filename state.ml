type action =
  | Help
  | Menu_Initial
  | Menu
  | Run_Algorithm
  | Sell_All
  | Refresh_and_Show
  | Logout
  | Quit
  | Optimize (* print time since last optimize, are you sure u'd like to optimize?*)
  | Configure
  | Configure_SR_Subreddits
  | Configure_SR_Posts
  | Configure_SR_Ordering
  | Configure_OP_Use
  | Configure_OP_Consts
  | Configure_OP_Tests
  | Graph
  | Graph_Networth
  | Graph_Liquidity
  | Graph_Networth_Liquidity
  | Graph_Stock
  | Account
  | Account_Change_Username
  | Account_Change_Email
  | Account_Change_Password
  | Account_Notification_On
  | Account_Notification_Off
  | Account_Delete

(* when performing any actions on user portfolio that return a new portfolio,
    save old portfolio to past_portfolios and set returned portfolio as current *)
(* when going into configure specific settings | print current settings  *)
type t = { auth : Auth.auth; mutable user : User.t; mutable state : action }

exception InvalidAction of (string * string)

exception InapplicableAction of (string * string)

exception QuitAction

exception LogoutAction

exception HelpAction

let init auth user = { auth; user; state = Menu }

let get_available_actions = function
  | Menu_Initial | Menu ->
      [
        Help;
        Menu;
        Menu_Initial;
        Configure;
        Run_Algorithm;
        Sell_All;
        Refresh_and_Show;
        Graph;
        Account;
        Logout;
        Quit;
      ]
  | Configure ->
      [
        Configure_SR_Subreddits;
        Configure_SR_Posts;
        Configure_SR_Ordering;
        Configure_OP_Use;
        Configure_OP_Consts;
        Configure_OP_Tests;
        Menu;
        Configure;
        Logout;
        Quit;
      ]
  | Graph ->
      [
        Graph_Networth;
        Graph_Liquidity;
        Graph_Networth_Liquidity;
        Graph_Stock;
        Graph;
        Menu;
        Logout;
        Quit;
      ]
  | Account ->
      [
        Account_Change_Username;
        Account_Change_Email;
        Account_Change_Password;
        Account_Notification_On;
        Account_Notification_Off;
        Account_Delete;
        Account;
        Menu;
        Logout;
        Quit;
      ]
  | _ -> []

let is_valid_action current_action action =
  List.mem action (get_available_actions current_action)

let string_of_action a =
  match a with
  | Menu | Menu_Initial -> "menu"
  | Configure -> "configure"
  | Graph -> "graph data"
  | Account -> "account"
  | _ -> failwith "not a *state* action"

let action_of_string_menu state s =
  if s = "help" then Help
  else if s = "menu initial" then Menu_Initial
  else if s = "menu" then Menu
  else if s = "configure" then Configure
  else if s = "run" then Run_Algorithm
  else if s = "sell all" then Sell_All
  else if s = "show data" then Refresh_and_Show
  else if s = "graph data" then Graph
  else if s = "account" then Account
  else if s = "logout" then Logout
  else if s = "quit" then Quit
  else raise (InvalidAction (s, string_of_action state.state))

let action_of_string_configure state s =
  if s = "runner subreddits" then Configure_SR_Subreddits
  else if s = "runner posts" then Configure_SR_Posts
  else if s = "runner ordering" then Configure_SR_Ordering
  else if s = "optimizer use" then Configure_OP_Use
  else if s = "optimizer tests" then Configure_OP_Tests
  else if s = "optimizer constants" then Configure_OP_Consts
  else raise (InvalidAction (s, string_of_action state.state))

let action_of_string_graph state s =
  if s = "graph net worth" then Graph_Networth
  else if s = "graph net worth and liquidity" then Graph_Networth_Liquidity
  else if s = "graph stock" then Graph_Stock
  else raise (InvalidAction (s, string_of_action state.state))

let action_of_string_account state s =
  if s = "change email" then Account_Change_Email
  else if s = "change password" then Account_Change_Password
  else if s = "change name" then Account_Change_Username
  else if s = "delete" then Account_Delete
  else if s = "toggle notifications" then Account_Notification_Off
  else raise (InvalidAction (s, string_of_action state.state))

let action_of_string state s =
  let s' = String.lowercase_ascii s in
  let result =
    try action_of_string_menu state s'
    with InvalidAction s -> (
      try action_of_string_configure state s'
      with InvalidAction s -> (
        try action_of_string_graph state s'
        with InvalidAction s -> action_of_string_account state s'))
  in
  if is_valid_action state.state result then result
  else raise (InapplicableAction (s, string_of_action state.state))

let menu ?(initial = false) state =
  if initial = false then
    ANSITerminal.print_string [ ANSITerminal.magenta ]
      "\nOStocker Actions Menu:\n\n"
  else if state.auth = Login then
    ANSITerminal.print_string [ ANSITerminal.magenta ]
      ("\nWelcome back " ^ User.name state.user
     ^ "! In case you forgot, here are your options:\n\n")
  else
    ANSITerminal.print_string [ ANSITerminal.magenta ]
      ("\nWelcome, " ^ User.name state.user
     ^ "! Below you can see all of the options available to you.\n\n");

  print_endline
    "\"help\" : walk you through how I works and how to use me\n\
     \"configure\" : configure my subreddit and optimization settings\n\
     \"run\" : run my algorithm and buy / sell stocks accordingly \n\
     \"sell all\" : sell all of your owned stocks\n\
     \"show data\" : show text data about your portfolio\n\
     \"graph data\" : graph data about your portfolio\n\
     \"account\" : view and change account settings\n\
     \"menu\" : show this menu\n\
     \"logout\" : logout\n\
     \"quit\" : quit\n";
  state.state <- Menu;
  ()

let configure state =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\nOStocker Configurations Menu:\n\n";
  print_endline
    "\"runner subreddits\" : lets you configure what subreddits you want the \
     runner to scrape\n\
     \"runner posts\" : lets you configure how many posts are scraped by a \
     certain subreddit\n\
     \"runner ordering\" : lets you configure the subreddit scaping order of a \
     certain subreddit  \n\
     \"optimizer use\" : lets you configure whether or not the optimizer is \
     enabled\n\
     \"optimizer tests\" : lets you configure the amount of tests you want to \
     do one each constant in optimization\n\
     \"optimizer constants\" : lets you configure the optimizer constants.\n\
     \"menu\" : return to main menu\n\
     \"logout\" : logout\n\
     \"quit\" : quit\n";
  state.state <- Configure;
  ()

let graph state =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\nOStocker Grapher and Data Visualization Menu:\n\n";
  print_endline
    "\"graph net worth\" : graphs your net worth on side by side graphs\n\
     \"graph net worth and liquidity\" : graphs your net worth added to your \
     liquidity on side by side graphs\n\
     \"graph stock\" : lets you graph the evaluation of your holding in a \
     stock  \n\
     \"menu\" : return to main menu\n\
     \"logout\" : logout\n\
     \"quit\" : quit\n";
  state.state <- Graph;
  ()

let account state =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\nOStocker Account Settings Menu:\n\n";
  print_endline
    "\"change email\" : lets you change your email\n\
     \"change name\" : lets you change your name\n\
     \"change password\" : lets you change your password\n\
     \"delete\" : lets you delete your account  \n\
     \"toggle notifications\" : lets you toggle on/off email notifications  \n\
     \"menu\" : return to main menu\n\
     \"logout\" : logout\n\
     \"quit\" : quit\n";
  state.state <- Account;
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

let fst4 (x, _, _, _) = x

let snd4 (_, x, _, _) = x

let thd4 (_, _, x, _) = x

let fth4 (_, _, _, x) = x

let run_algorithm state =
  let porfolio = User.current_portfolio state.user in
  let config = User.config state.user in
  let constants = Config.consts config in
  let subreddit_list = Config.subreddit_info config in
  print_endline "starting scrape";
  let scraped_subreddit_list =
    List.map
      (fun (amount, order, subreddit) ->
        Scraper.scrape subreddit ~amount ~ordering:order)
      subreddit_list
  in
  let weighted =
    Algorithm.get_stocks_consts (fst4 constants) (snd4 constants)
      (thd4 constants) (fth4 constants) scraped_subreddit_list
  in
  let new_portfolio = Portfolio.process porfolio weighted in
  state.user <- User.update_portfolio state.user new_portfolio;
  ()

let update state action =
  match action with
  (* time_for_daily_tasks will return true if call optizimer function or not in alg *)
  | Sell_All -> () (* call set portfolio to result of portfolio.sell_all *)
  | Help -> help ()
  | Menu_Initial -> menu state ~initial:true
  | Menu -> menu state
  | Logout -> raise LogoutAction
  | Quit -> raise QuitAction
  | Configure -> configure state
  | Graph -> graph state
  | Account -> account state
  | Run_Algorithm -> run_algorithm state
  | Refresh_and_Show | Configure_SR_Subreddits | Configure_SR_Posts
  | Configure_SR_Ordering | Configure_OP_Use | Configure_OP_Consts
  | Configure_OP_Tests | Graph_Networth | Graph_Liquidity
  | Graph_Networth_Liquidity | Graph_Stock | Account_Change_Username
  | Account_Change_Email | Account_Change_Password | Account_Notification_On
  | Account_Notification_Off | Account_Delete ->
      failwith ""
  | _ -> ()

let user state = state.user

let config state = User.config state.user

let portfolio state = User.current_portfolio state.user

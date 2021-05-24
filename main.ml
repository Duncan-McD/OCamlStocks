(* [load email] is the [State.t] loaded with [email] *)
let load email = State.init

(** [save] saves [state] with given [email] *)
let save email state = ()

(* [display_menu auth] prints the action menu *)
let display_menu ?(requested = false) auth =
  if requested then
    ANSITerminal.print_string [ ANSITerminal.magenta ]
      "\nOStocker Actions Menu:\n\n"
  else if auth = Auth.Login then
    ANSITerminal.print_string [ ANSITerminal.magenta ]
      "\nWelcome back! In case you forgot, here are your options:\n\n"
  else
    ANSITerminal.print_string [ ANSITerminal.magenta ]
      "\nBelow you can see all of the options available to you.\n\n";

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
  print_string "Welcome to the Help Page! Here is a little tutorial of how to use this program. (press ENTER to continue or type anything and ENTER to quit)";
  let help_instructions = [
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
  ] in
  let pause_escape str = 
    match read_line () with
    | exception End_of_file -> ()
    | "" -> print_string str
    | _ -> print_endline @@ border ^ "\n"; raise (Failure "quit help") in
  try 
    List.iter pause_escape help_instructions;
    print_endline @@ "\n" ^ border ^ "\n" 
  with
  | Failure _ -> ()
  | _ -> ()

(** [main ()] is the program that allows the user to interact with the bot. *)
let rec main prompt =
  if prompt = Auth.Initial_Prompt then
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\n\
       Hi there! My name is OStocker, I am an OCAML Stocks Bot.\n\n\
       Using my top-of-the line algorithm and your choice of subreddit data,\n\
       I will invest in the best stocks for you! \n\n";

  (* Load data *)
  try
    let user = Auth.prompt_user prompt in
    let auth_type = Auth.auth_type user in
    let email = Auth.email user in
    let state = load email in

    (* display menu *)
    let () = display_menu auth_type in

    (* while user does not type "quit" or "q" keep runing *)
    let quit_loop = ref false in
    while not !quit_loop do
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | input -> (
          if input != "" then
            try
              let action = State.action_of_string input in
              State.update state action
            with
            | State.MenuAction -> display_menu ~requested:true auth_type
            | State.HelpAction -> help ()
            | State.InvalidAction s ->
                ANSITerminal.print_string [ ANSITerminal.red ]
                  ( "\nInvalid action: \"" ^ s
                  ^ "\". You can type \"menu\" to see your options.\n" )
            | State.LogoutAction ->
                save email state;
                main Auth.Logged_Out
            | State.QuitAction -> quit_loop := true )
    done;

    save email state;
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\n\
       Thanks for using OCamlStocks! I look forward to working with you again.\n"
  with Auth.QuitException -> ()

(* Execute the bot engine. *)
let () = main Auth.Initial_Prompt

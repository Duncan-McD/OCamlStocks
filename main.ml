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

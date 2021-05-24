(* [load auth user] is the [State.t] loaded with [user] and [auth] *)
let load auth_type user = State.init auth_type user

(** [save] saves [state] with given [user] *)
let save (state : State.t) = Saveload.save_user state.user

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
    let auth = Auth.prompt_user prompt in
    let auth_type = Auth.auth_type auth in
    let user = Auth.user auth in
    let state = load auth_type user in

    (* display menu *)
    let () = State.update state (State.action_of_string state "menu initial") in

    (* while user does not type "quit" or "q" keep runing *)
    let quit_loop = ref false in
    while not !quit_loop do
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | input -> (
          if input <> "" then
            try
              let action = State.action_of_string state input in
              State.update state action
            with
            | State.InvalidAction (s, string_state) ->
                ANSITerminal.print_string [ ANSITerminal.red ]
                  ("\nInvalid action: \"" ^ s ^ "\". You can type \""
                 ^ string_state ^ "\" to see your options.\n")
            | State.InapplicableAction (s, string_state) ->
                ANSITerminal.print_string [ ANSITerminal.red ]
                  ("\n\"" ^ s ^ "\" cannot be used here. You can type \""
                 ^ string_state ^ "\" to see your options.\n")
            | State.LogoutAction ->
                save state;
                main Auth.Logged_Out
            | State.QuitAction -> quit_loop := true)
    done;

    save state;
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\n\
       Thanks for using OCamlStocks! I look forward to working with you again.\n"
  with Auth.QuitException -> ()

(* Execute the bot engine. *)
let () = main Auth.Initial_Prompt

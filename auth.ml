type prompt =
  | Initial_Prompt
  | Logged_Out
  | Invalid_Input
  | Empty_Input
  | Already_Taken
  | Changed

exception QuitException

type auth = Login | Signup

type t = auth * User.t

let auth_type a = fst a

let user a = snd a

let auth_temp (email, password) =
  if email = "invalid@invalid.com" then false else true

let rec prompt_user prompt =
  if prompt = Initial_Prompt || prompt = Empty_Input then
    print_endline
      "\n\
       Type \"login\" or \"signup\", to get started. If you would like to \
       quit, type \"quit\"."
  else if prompt = Logged_Out then
    let () =
      ANSITerminal.print_string [ ANSITerminal.green ]
        "\nYou have been sucessfully logged out.\n"
    in

    print_endline
      "\n\
       Type \"login\" or \"signup\", to go back. If you would like to quit, \
       type \"quit\"."
  else if prompt = Invalid_Input then
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\n\
       Invalid input. Please type \"login\" or \"signup\". If you would like \
       to quit type \"quit\".\n"
  else
    print_endline
      "\n\
       Type \"login\" or \"signup\". If you would like to quit, type \"quit\".\n";

  print_string "> ";

  match read_line () with
  | exception End_of_file -> failwith "Error reading input"
  | input ->
      let input = String.lowercase_ascii input in
      if input = "" then prompt_user Empty_Input
      else if input = "login" then login Initial_Prompt
      else if input = "signup" then signup Initial_Prompt
      else if input = "quit" then raise QuitException
      else prompt_user Invalid_Input

and (* [login ()] is the user email once they have logged in *)
    login prompt =
  if prompt = Invalid_Input then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\n\
       Invalid login credentials. Type anything (e.g. \"back\") to go back, or \
       press enter to try again.\n";

    correct_input Login )
  else (
    print_string "\nName: ";

    match read_line () with
    | exception End_of_file -> failwith "Error reading input."
    | name -> (
        print_string "Email: ";

        match read_line () with
        | exception End_of_file -> failwith "Error reading input."
        | email -> (
            print_string "Password: ";

            match read_line () with
            | exception End_of_file -> failwith "Error reading input."
            | password ->
                if auth_temp (email, password) then
                  (Login, User.create email name password)
                else login Invalid_Input ) ) )

and (* [signup ()] is the user email once they have signed up *)
    signup prompt =
  if prompt = Already_Taken then
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\n\
       The email address provided is already in use. Type anything (e.g. \
       \"back\") to go back, or press enter to try again.\n"
  else if prompt = Invalid_Input then
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\n\
       Invalid email or password. Type anything (e.g. \"back\") to go back, or \
       press enter to try again.\n";

  if prompt = Already_Taken || prompt = Invalid_Input then correct_input Signup
  else (
    print_string "\nName: ";

    match read_line () with
    | exception End_of_file -> failwith "Error reading input."
    | name -> (
        print_string "Email: ";

        match read_line () with
        | exception End_of_file -> failwith "Error reading input."
        | email -> (
            print_string "Password: ";

            match read_line () with
            | exception End_of_file -> failwith "Error reading input."
            | password ->
                if auth_temp (email, password) then
                  (Signup, User.create email name password)
                else signup Invalid_Input ) ) )

and correct_input login_or_signup =
  print_string "> ";

  match read_line () with
  | exception End_of_file -> failwith "Error reading input."
  | input ->
      if input <> "" then prompt_user Changed
      else if login_or_signup = Login then login Changed
      else signup Changed

<<<<<<< HEAD
(** [load] is a [State.t] with saved data or [State.init] if none exists *)
let load () = State.init

(** [save] saves [state] *)
let save sate = ()
=======
let get_user = "user1"

(** [load] is a [State.t] with saved data or [State.init] if none exists *)
let load () = Saveload.load_user_state get_user

(** [save] saves [state] *)
let save sate = Saveload.save_user_state get_user
>>>>>>> main

(** [main ()] is the program that allows the user to interact with the bot. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nHi there! My name is OStocker, I am an OCAML Stocks Bot.\n";

  print_endline "Will display stuff soon.\n";

  (* ====== Load previous data if exists here ===== *)
  let state = load () in

  (* while user does not type "quit" or "q" keep runing *)
  let quit_loop = ref false in
  while not !quit_loop do
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | input -> (
        try
          let action = State.action_of_string input in
          State.update state action
        with
        | State.InvalidAction s ->
            print_endline ("Invalid action: \"" ^ s ^ "\"")
<<<<<<< HEAD
        | State.QuitAction -> quit_loop := true )
=======
        | State.QuitAction -> quit_loop := true)
>>>>>>> main
  done

(* once user quits, run exit code and quit *)
let exit = ()

(* Execute the bot engine. *)
let () = main ()

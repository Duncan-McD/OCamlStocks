(** [main ()] is the program that allows the user to interact with the bot. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nHi there! My name is OStocker, I am an OCAML Stocks Bot.\n";
  
    print_endline
    "Will display stuff soon.\n";

  print_string "> ";

  (* while user does not type "quit" or "q" keep runing *)
  let quit_loop = ref false in
  while not !quit_loop do
    match read_line () with
    | exception End_of_file -> ()
    | input -> 
      if input = "quit" || input = "q" then quit_loop := true 
      else print_string "> ";
  done

  (* once user quits, run exit code and quit *)
  let exit = ()

(* Execute the bot engine. *)
let () = main ()
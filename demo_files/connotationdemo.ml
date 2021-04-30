let ask_for_string () = 
  print_endline "Enter a string to check its connotation";
  read_line () 

let rec ask_if_want_another_check () =
  print_endline "Do you want to check another string? [Y/N]";
  let response = read_line () in
  if response = "Y" then true
  else if response = "N" then false
  else (
    print_endline "Invalid Input";
    ask_if_want_another_check ())

let rec main () =
  let words = ask_for_string() in
  print_endline "Connotation: ";
  print_float (Parser.connotation_str words);
  print_endline "";
  let go_again = ask_if_want_another_check() in
  if go_again then main () else ()



let () = main ()
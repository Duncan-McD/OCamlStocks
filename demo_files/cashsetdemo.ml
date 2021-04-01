open Cashset

let rec prompt_for_stock () =
  print_endline "Enter a string/word to check if it is a stock ticker";
  let word = read_line () in
  if is_stock_name word then (
    print_endline (word ^ " is a stock ticker");
    ask_if_want_new ())
  else (
    print_endline (word ^ " is not a stock ticker");
    ask_if_want_new ())

and ask_if_want_new () =
  print_endline "Do you want to check another string? [Y/N]";
  let response = read_line () in
  if response = "Y" then prompt_for_stock ()
  else if response = "N" then ()
  else (
    print_endline "Invalid Input";
    ask_if_want_new ())

let main () = prompt_for_stock ()

let () = main ()

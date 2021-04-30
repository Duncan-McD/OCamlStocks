let ask_for_subreddit () = 
  print_endline "Please enter the name of a subreddit in the form r/subreddit - Example: \
  r/wallstreetbets not \"r/wallstreetbets\"";
  let ticker = read_line () in
  (ticker, ticker |> Stockdata.stockdata_from_ticker)

let rec ask_if_want_another_check () =
  print_endline "Do you want to check another string? [Y/N]";
  let response = read_line () in
  if response = "Y" then true
  else if response = "N" then false
  else (
    print_endline "Invalid Input";
    ask_if_want_another_check ())

let rec main () =
  let scraped = ask_for_subreddit() in 
  let stock_is_none = match stock with 
  | None -> true
  | _ -> false in
  print_endline(ticker ^ " Recommendation Rating: ");
  print_float (if stock_is_none then 3. else stock |> Stockdata.require |> Stockdata.rating);
  print_endline("");
  print_endline(ticker ^ " History Score: ");
  print_float(if stock_is_none then Parser.history_score None else Parser.history_score stock);
  print_endline("");
  let go_again = ask_if_want_another_check() in
  if go_again then main () else ()



let () = main ()
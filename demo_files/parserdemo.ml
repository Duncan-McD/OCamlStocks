open Scraper

let rec get_scrape_time subreddit amount =
  print_endline
    "what time do u want to request from? \"Now\", \"Today\", \"ThisWeek\", \
     \"ThisMonth\", \"ThisYear\", or \"AllTime\"? Note: do not include the \
     quotes";
  let time = read_line () in
  if time = "Now" then scrape subreddit ~amount ~ordering:(Top Now)
  else if time = "Today" then scrape subreddit ~amount ~ordering:(Top Today)
  else if time = "ThisWeek" then
    scrape subreddit ~amount ~ordering:(Top ThisWeek)
  else if time = "ThisMonth" then
    scrape subreddit ~amount ~ordering:(Top ThisMonth)
  else if time = "ThisYear" then
    scrape subreddit ~amount ~ordering:(Top ThisYear)
  else if time = "AllTime" then scrape subreddit ~amount ~ordering:(Top AllTime)
  else (
    print_endline "Invalid input";
    get_scrape_time subreddit amount)

let rec get_scrape subreddit amount =
  print_endline
    "What order do u want to request from reddit? \"Hot\", \"New\", \
     \"Rising\", or \"Top\"? Note: do not include the quotes";
  let order = read_line () in
  let scraped_subreddit =
    if order = "New" then scrape subreddit ~amount ~ordering:New
    else if order = "Rising" then scrape subreddit ~amount ~ordering:Rising
    else if order = "Hot" then scrape subreddit ~amount ~ordering:Hot
    else if order = "Top" then get_scrape_time subreddit amount
    else (
      print_endline "Invalid input";
      get_scrape subreddit amount)
  in
  scraped_subreddit

let rec pretty_print_parsed_text stocks =
  match stocks with
  | [] -> ()
  | h :: t ->
      print_endline h;
      pretty_print_parsed_text t

let print_data data =
  print_endline ("CONNOTATION: " ^ string_of_float (Parser.connotation data));
  print_endline ("RATIO: " ^ string_of_float (Parser.upvote_ratio data));
  print_endline ("SCORE: " ^ string_of_int (Parser.upvote_score data))

let rec pretty_print_data_parse_list datas =
  match datas with
  | [] -> ()
  | h :: t ->
      print_data h;
      pretty_print_data_parse_list t

let rec ask_for_pretty_print_data_parse_list datas =
  print_endline "Would you like to print this data? [Y/N]";
  let response = read_line () in
  if response = "Y" then pretty_print_data_parse_list datas
  else if response = "N" then ()
  else (
    print_endline "Invalid Input";
    ask_for_pretty_print_data_parse_list datas)

let rec ask_for_look_ups subreddit =
  print_endline "Would you like to get the data of any of these stocks? [Y/N]";
  let response = read_line () in
  if response = "Y" then ask_for_stock subreddit
  else if response = "N" then ()
  else (
    print_endline "Invalid Input";
    ask_for_look_ups subreddit)

and ask_for_stock subreddit =
  print_endline "Enter the name of one of the stocks to get its data: ";
  let response = read_line () in
  if List.mem response (Parser.stock_names subreddit) then (
    ask_for_pretty_print_data_parse_list (snd (Parser.data subreddit response));
    ask_for_look_ups subreddit)
  else (
    print_endline "This was not a stock option. Try again.";
    ask_for_stock subreddit)

let rec ask_to_parse subreddit =
  print_endline
    "Would you like to parse and then pretty print this subreddit? [Y/N]";
  let response = read_line () in
  if response = "Y" then (
    let parsed_subreddit = Parser.parse subreddit in
    let stock_names = Parser.stock_names parsed_subreddit in
    pretty_print_parsed_text stock_names;
    ask_for_look_ups parsed_subreddit)
  else if response = "N" then ()
  else (
    print_endline "Invalid Input";
    ask_to_parse subreddit)

let rec ask_for_another_scrape () =
  print_endline "would you like to scrape another subreddit? [Y/N]";
  let response = read_line () in
  if response = "Y" then (
    try do_scrape () with
    | TooManyPostsRequested i ->
        print_endline ("You requested " ^ string_of_int i ^ " too many posts");
        print_endline "Trying again";
        do_scrape ()
    | SubredditNotFound s ->
        print_endline "You requested a subreddit that does not exist";
        print_endline "Trying again";
        do_scrape ())
  else if response = "N" then ()
  else (
    print_endline "Invalid Input";
    ask_for_another_scrape ())

and do_scrape () =
  print_endline
    "Please enter the name of a subreddit in the form r/subreddit - Example: \
     r/wallstreetbets not \"r/wallstreetbets\"";
  print_endline
    "Subreddits based around the stock market are reccomended as they will \
     produce better data for Parser";
  print_endline
    "Examples: r/wallstreetbets, r/stocks, r/investing, r/options, \
     r/economics, r/personalfinance";
  let subreddit = read_line () in
  print_endline "how many posts do u want to scrape?";
  let amount = read_int () in

  let scraped_subreddit = get_scrape subreddit amount in
  ask_to_parse scraped_subreddit;
  ask_for_another_scrape ()

let rec main () =
  print_endline "";
  print_endline
    "First we will build the subreddit using Scraper that we will then later \
     parse";
  print_endline "";
  try do_scrape () with
  | TooManyPostsRequested i ->
      print_endline ("You requested " ^ string_of_int i ^ " too many posts");
      print_endline "Trying again";
      do_scrape ()
  | SubredditNotFound s ->
      print_endline "You requested a subreddit that does not exist";
      print_endline "Trying again";
      do_scrape ()

let () = main ()

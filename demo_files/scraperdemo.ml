open Scraper

let rec print_post posts =
  match posts with
  | [] -> ()
  | h :: t ->
      print_endline "";
      print_endline ("POST TITLE: " ^ title h);
      print_endline ("POST BODY: " ^ body h);
      print_endline ("POST SCORE: " ^ string_of_int (score h));
      print_endline ("POST UPVOTE RATIO: " ^ string_of_float (upvote_ratio h));
      print_post t

let rec ask_for_pretty_print (subreddit : subreddit) : unit =
  print_endline "Do you want to pretty print the output of this scrape? [Y/N]";
  let response = read_line () in
  if response = "Y" then (
    print_endline ("SUBREDDIT NAME: " ^ subreddit_name subreddit);
    print_post (posts subreddit))
  else if response = "N" then ()
  else (
    print_endline "Invalid Input";
    ask_for_pretty_print subreddit)

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
    "please enter the name of a subreddit in the form r/subreddit - Example: \
     r/wallstreetbets not \"r/wallstreetbets\"";
  let subreddit = read_line () in
  print_endline "how many posts do u want to scrape?";
  let amount = read_int () in

  let scraped_subreddit = get_scrape subreddit amount in
  ask_for_pretty_print scraped_subreddit;
  ask_for_another_scrape ()

let rec main () =
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

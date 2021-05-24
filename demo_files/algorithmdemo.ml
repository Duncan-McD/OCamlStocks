(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_float f] pretty-prints float [f]. *)
let pp_float = string_of_float

(** [pp_assoc_list pp_elt lst] pretty-prints association list [lst],
    using [pp_elt] to pretty-print each tuple of elements in [lst]. *)
let pp_assoc_list pp_key_elt pp_val_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ (k, v) ] -> acc ^ "(" ^ pp_key_elt k ^ "," ^ pp_val_elt v ^ ")"
      | (k1, v1) :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else
            loop (n + 1)
              (acc ^ "(" ^ pp_key_elt k1 ^ "," ^ pp_val_elt v1 ^ ")" ^ "; ")
              t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let rec get_scrape_time subreddit amount =
  print_endline
    "what time do u want to request from? \"Now\", \"Today\", \"ThisWeek\", \
     \"ThisMonth\", \"ThisYear\", or \"AllTime\"? Note: do not include the \
     quotes";
  let time = read_line () in
  if time = "Now" then Scraper.scrape subreddit ~amount ~ordering:(Top Now)
  else if time = "Today" then
    Scraper.scrape subreddit ~amount ~ordering:(Top Today)
  else if time = "ThisWeek" then
    Scraper.scrape subreddit ~amount ~ordering:(Scraper.Top Scraper.ThisWeek)
  else if time = "ThisMonth" then
    Scraper.scrape subreddit ~amount ~ordering:(Scraper.Top Scraper.ThisMonth)
  else if time = "ThisYear" then
    Scraper.scrape subreddit ~amount ~ordering:(Scraper.Top Scraper.ThisYear)
  else if time = "AllTime" then
    Scraper.scrape subreddit ~amount ~ordering:(Scraper.Top Scraper.AllTime)
  else (
    print_endline "Invalid input";
    get_scrape_time subreddit amount )

let rec get_scrape subreddit amount =
  print_endline
    "What order do u want to request from reddit? \"Hot\", \"New\", \
     \"Rising\", or \"Top\"? Note: do not include the quotes";
  let order = read_line () in
  let scraped_subreddit =
    if order = "New" then Scraper.scrape subreddit ~amount ~ordering:New
    else if order = "Rising" then
      Scraper.scrape subreddit ~amount ~ordering:Rising
    else if order = "Hot" then Scraper.scrape subreddit ~amount ~ordering:Hot
    else if order = "Top" then get_scrape_time subreddit amount
    else (
      print_endline "Invalid input";
      get_scrape subreddit amount )
  in
  scraped_subreddit

let rec ask_for_scrape () =
  try do_scrape () with
  | Scraper.TooManyPostsRequested i ->
      print_endline ("You requested " ^ string_of_int i ^ " too many posts");
      print_endline "Trying again";
      do_scrape ()
  | Scraper.SubredditNotFound s ->
      print_endline "You requested a subreddit that does not exist";
      print_endline "Trying again";
      do_scrape ()

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

  get_scrape subreddit amount

let rec ask_if_want_another_check () =
  print_endline "Do you want to check another subreddit? [Y/N]";
  let response = read_line () in
  if response = "Y" then true
  else if response = "N" then false
  else (
    print_endline "Invalid Input";
    ask_if_want_another_check () )

let rec main () =
  let scraped = ask_for_scrape () in
  let algoed = fst @@ Algorithm.get_stocks [ scraped ] in
  let () = print_endline (pp_assoc_list pp_string pp_float algoed) in
  let go_again = ask_if_want_another_check () in
  if go_again then main () else ()

let () = main ()

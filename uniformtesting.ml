open Scraper
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
    get_scrape_time subreddit amount)

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
      get_scrape subreddit amount)
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
    ask_if_want_another_check ())

(*2 -> 1 3-> 1/2, 4-> 1/3, 5 -> 1/4... n -> 1/(n-1)*)
let p n x max= match n with
| 0 -> 0.
| 1 -> float_of_int x
| i -> float_of_int x *. (max /. float_of_int (n - 1))

(*let scraped1 = ask_for_scrape ()*)
let test_var : ((string * float) list * string list * (float * float * float * float)) = ([], [], (0., 0., 0., 0.))

let int_arr_test : int array = Array.make 20 0

let const_and_stocks_to_buy_array : ((string * float) list * string list * (float * float * float * float)) array = Array.make 20 test_var

let current_poss = ref 0

let add_to_array (arr : ((string * float) list * string list * (float * float * float * float)) array) list_and_constants = 
  arr.(!current_poss) <- list_and_constants; current_poss := !current_poss + 1
  

(*Notes about how to fix: 
  TODO: make array of type (((string * float list) * string list) * (float * float * float * float))
  figure out what is calling test_n_per_var, for example if user calls "optimize constants"
  possible way of functionality:
  test_n_per_var 

*)

let compare_array_spots (a1 : ((string * float) list * string list * (float * float * float * float)) (a2 : (((string * float) list * string list) * (float * float * float * float)) = 
  let a1_gains = Portfolio.get_

let test_n_per_var n =
  let scraped2 = Scraper.scrape_json ~amount:25 "testing_files/stocksnew.json" in
  let parsed = Parser.parse scraped2 in
  for score_con = 1 to n do
    for con_const = 1 to n do
      for  num_posts_const= 1 to n do
        for hist_const = 1 to n do
          let score_con' = (p n score_con 2.) in 
          let con_const' = (p n con_const 2.) in 
          let num_posts_const' = (p n num_posts_const 2.) in 
          let hist_const' = (p n hist_const 2.) in 
          let algoed =
            Algorithm.get_stocks_consts score_con' con_const' num_posts_const' hist_const' parsed
          in
          add_to_array const_and_stocks_to_buy_array (algoed, (score_con, con_const, num_posts_const, hist_const)); 
          print_endline (pp_assoc_list pp_string pp_float (fst algoed))
        done
      done
    done
  done;
  Array.sort const_and_stocks_to_buy_array


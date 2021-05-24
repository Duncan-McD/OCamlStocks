let p n x max =
  match n with
  | 0 -> 0.
  | 1 -> float_of_int x
  | i -> float_of_int x *. (max /. float_of_int (n - 1))

let rec get_scraped_list
    (sub_list : (int * Scraper.subreddit_ordering * string) list)
    (acc : Scraper.subreddit list) =
  match sub_list with
  | [] -> acc
  | (num_posts, ordering, name) :: t ->
      get_scraped_list t (Scraper.scrape ~amount:num_posts name :: acc)

let initialize_testing_portfolios (user : User.t) =
  let config = User.config user in
  let n =
    int_of_float
      (Float.sqrt (Float.sqrt (float_of_int (Config.num_tests config))))
  in
  let scraped_list = get_scraped_list (Config.subreddit_info config) [] in
  let testing_portfolio_list = ref [] in
  for score_con = 1 to n do
    for con_const = 1 to n do
      for num_posts_const = 1 to n do
        for hist_const = 1 to n do
          let score_con' = p n score_con 2. in
          let con_const' = p n con_const 2. in
          let num_posts_const' = p n num_posts_const 2. in
          let hist_const' = p n hist_const 2. in
          let algoed =
            Algorithm.get_stocks_consts score_con' con_const' num_posts_const'
              hist_const' scraped_list
          in
          testing_portfolio_list :=
            Portfolio.process Portfolio.empty_portfolio algoed
            :: !testing_portfolio_list
        done
      done
    done
  done;
  User.change_test_portfolios user !testing_portfolio_list

let optimized_constants (user : User.t) : float * float * float * float =
  (*THIS IS A BAD SOLUTION!! TODO: set up hashtable to store stock data from yahoo finance to save time
    TODO: figure out where to send constants
      SetOptimizer constants *)
  Portfolio.vars
    (List.hd
       (List.sort Portfolio.compare
          (List.map Portfolio.refresh (User.test_portfolios user))))

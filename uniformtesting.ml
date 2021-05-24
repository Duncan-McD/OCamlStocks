let p n x max =
  match n with
  | 0 -> 0.
  | 1 -> float_of_int x
  | i -> float_of_int x *. (max /. float_of_int (n - 1))

let rec get_scraped_list (config : Config.t) (subreddit_list : string list)
    (acc : Scraper.subreddit list) =
  match subreddit_list with
  | [] -> acc
  | h :: t ->
      get_scraped_list config t
        (Scraper.scrape ~amount:(Config.posts_per_scrape config) h :: acc)

let initialize_testing_portfolios =
  let config = State.config State.init in
  let n =
    int_of_float
      (Float.sqrt (Float.sqrt (float_of_int (Config.number_of_tests config))))
  in
  let scraped_list =
    get_scraped_list config (Config.subreddit_list config) []
  in
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
          let portfolio = Portfolio.process Portfolio.empty_portfolio algoed in
          Saveload.save_testing_portfolio portfolio
        done
      done
    done
  done

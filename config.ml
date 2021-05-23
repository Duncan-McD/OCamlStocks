type t = {
  number_of_tests : int;
  subreddit_list : string list;
  posts_per_scrape : int;
}

let default =
  {
    number_of_tests = 1000;
    subreddit_list = [ "r/stocks"; "r/investing" ];
    posts_per_scrape = 100;
  }

let number_of_tests (config : t) : int = config.number_of_tests

let subreddit_list (config : t) : string list = config.subreddit_list

let posts_per_scrape (config : t) : int = config.posts_per_scrape

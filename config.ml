type subreddit = int * Scraper.subreddit_ordering * string

type t = {
  subreddits : subreddit list;
  optimizing : bool;
  consts : float * float * float * float;
  num_test : int;
  liquidity : float;
}

let default () = {
  subreddits = [ (50, Hot, "r/stocks"); (50, Hot, "r/investing") ];
  optimizing = true;
  consts = (1., 1., 1., 1.);
  num_test = 10000;
  liquidity = 0.;
}

let add_subreddit config subreddit = {
  subreddits = subreddit :: config.subreddits;
  optimizing = config.optimizing;
  consts = config.consts;
  num_test = config.num_test;
  liquidity = config.liquidity;
}

let set_subreddits config new_subreddits = {
  subreddits = new_subreddits;
  optimizing = config.optimizing;
  consts = config.consts;
  num_test = config.num_test;
  liquidity = config.liquidity;
}

let set_optimizing config b = {
  subreddits = config.subreddits;
  optimizing = b;
  consts = config.consts;
  num_test = config.num_test;
  liquidity = config.liquidity;
}

let set_consts config (a, b, c, d) = {
  subreddits = config.subreddits;
  optimizing = config.optimizing;
  consts = (a, b, c, d);
  num_test = config.num_test;
  liquidity = config.liquidity;
}

let set_tests config tests = {
  subreddits = config.subreddits;
  optimizing = config.optimizing;
  consts = config.consts;
  num_test = tests;
  liquidity = config.liquidity;
}

let set_liquidity config new_liquidity = {
  subreddits = config.subreddits;
  optimizing = config.optimizing;
  consts = config.consts;
  num_test = config.num_test;
  liquidity = new_liquidity;
}

let subreddit_info config = config.subreddits

let is_optimizing config = config.optimizing

let consts config = config.consts

let num_tests config = config.num_test

let liquidity config = config.liquidity
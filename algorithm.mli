(** Processes parsed stock data list into a weighted stock list *)

val get_processed_stock_list :
  float -> float -> float -> float -> Scraper.subreddit -> (string * float) list

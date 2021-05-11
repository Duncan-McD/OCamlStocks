(** [get_processed_stock_list x y q w subreddit] is the weighted list of stocks
    mentioned within [subreddit] with constants [x], [y], [q], and [w]. The
    weights correspond to the recommended percentage of money to be invested in
    each particular stock. *)
val get_processed_stock_list :
  float -> float -> float -> float -> Scraper.subreddit -> (string * float) list

val get_stocks_consts :
  float ->
  float ->
  float ->
  float ->
  Scraper.subreddit list ->
  (string * float) list * string list
(** [get_stocks_consts x y q w subreddit_list] is a pair of lists where the 
    first element is a weighted list of stocks where the weights are at most
    1 and
    correspond to the recommended percentage of money to be invested in each
    particular stock. The second entry is a list of stocks recommended to be
    sold. The stocks found in the pair are all stocks mentioned within
    [subreddit_list] and processed using constants [x], [y], [q], and [w]. *)

<<<<<<< HEAD
val get_stocks : Scraper.subreddit -> (string * float) list * string list
(** [get_stocks subreddit] is a pair of lists where the first
=======
val get_stocks : Scraper.subreddit list -> (string * float) list * string list
(** [get_stocks subreddit_list] is a pair of lists where the first
>>>>>>> bot_runner
    element is a weighted list of stocks where the weights are at most 1 and
    correspond to the recommended percentage of money to be invested in each
    particular stock. The second entry is a list of stocks recommended to be
    sold. The stocks found in the pair are all stocks mentioned within
<<<<<<< HEAD
    [subreddit] and processed using optimized constants. *)
=======
    [subreddit_list] and processed using optimized constants. *)
>>>>>>> bot_runner

val get_stocks_consts :
  float ->
  float ->
  float ->
  float ->
<<<<<<< HEAD
  Parser.stocks ->
  (string * float) list * string list
(** [get_stocks_consts x y q w subreddit] is a pair of lists where the first
=======
  Scraper.subreddit list ->
  (string * float) list * string list
(** [get_stocks_consts x y q w subreddit_list] is a pair of lists where the first
>>>>>>> bot_runner
    element is a weighted list of stocks where the weights are at most 1 and
    correspond to the recommended percentage of money to be invested in each
    particular stock. The second entry is a list of stocks recommended to be
    sold. The stocks found in the pair are all stocks mentioned within
<<<<<<< HEAD
    [subreddit] and processed using constants [x], [y], [q], and [w]. *)
=======
    [subreddit_list] and processed using constants [x], [y], [q], and [w]. *)
>>>>>>> bot_runner

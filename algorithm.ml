(** [post_val p x y] is the numerical post value of a single post calculated by
    the mathematical equation [s^x * c^(1/y)] where [s] is the score and [c] is
    the connotation of post [p].
    Returns a positive value if and only if both [s] and [c] are positive.
    If [y] is 0, then the equation will be calculated by [s^x * c^0]. *)
let post_val (h : Parser.post) (x : float) (y : float) =
  let score = float_of_int (Parser.upvote_score h) in
  let conn = Parser.connotation h in
  let exp = if y = 0. then 0. else 1. /. y in
  let sign = if score < 0. || conn < 0. then -1. else 1. in
  sign *. Float.pow (Float.abs score) x *. Float.pow (Float.abs conn) exp

(** [reddit_score posts x y acc] is [acc] plus the sum of all post values of the
    posts in [posts] with constants [x] and [y]. This represents the reddit
    score of a given stock according to the given list of posts it is mentioned
    in [posts]. *)
let rec reddit_score (post_list : Parser.post list) x y acc =
  match post_list with
  | [] -> acc
  | h :: t -> reddit_score t x y (acc +. post_val h x y)

(** [stock_score stocks x y q w name] is the numerical score of the stock with
    ticker symbol [name] with constants [x], [y], [q], and [w]. The score is
    calculated with the mathematical equation [average(post value) + (q*n) + 
    (w*h)] where [n] is the number of posts of the stock denoted by [name] and
    [h] is the history score of the stock.
    Requires: the stock denoted by ticker symbol [name] be found in [stocks]. *)
let stock_score list_of_stocks x y q w stock_name =
  let data = Parser.data list_of_stocks stock_name in
  let post_list = snd data in
  let num_posts = float_of_int (List.length post_list) in
  (reddit_score post_list x y 0. /. num_posts)
  +. (q *. num_posts)
  +. (w *. fst data)

(** [process_scores stocks parsed x y q w acc sum] is a pair with the first
    element being [acc] and the stock scores of all the stocks in [stocks] with
    constants [x], [y], [q], and [w] and the second element being [sum] plus the
    total sum of all postive stock scores. *)
let rec process_scores (list_of_stocks : string list)
    (parsed_subreddit : Parser.stocks) x y q w (acc : float list) sum =
  match list_of_stocks with
  | [] -> (acc, sum)
  | h :: t ->
      let curr_score = stock_score parsed_subreddit x y q w h in
      process_scores t parsed_subreddit x y q w (curr_score :: acc)
        (sum +. if curr_score > 0. then curr_score else 0.)

(* [sr_list_to_stocks_lists stocks_list subreddit_list] is [stocks_lists] with 
    every subrredit in [subreddit_list] parsed and added *)
let rec sr_list_to_stocks_list stocks_list = function
  | [] -> stocks_list
  | sr :: t -> sr_list_to_stocks_list (Parser.parse sr :: stocks_list) t

let get_stocks_consts x y q w subreddit_list =
  let stocks_list = sr_list_to_stocks_list [] subreddit_list in
  let parsed_subreddits = Parser.join_stocks stocks_list in
  let stocks_list = Parser.stock_names parsed_subreddits in
  let processed = process_scores stocks_list parsed_subreddits x y q w [] 0. in
  let pos_score_sum = snd processed in
  let decision_func (buy, sell) (stock, score) =
    if score > 0. then ((stock, score /. pos_score_sum) :: buy, sell)
    else (buy, stock :: sell)
  in
  List.fold_left decision_func ([], [])
    (List.combine stocks_list (fst processed))

let get_stocks subreddit_list =
  let x, y, q, w = Optimizer.constants () in
  get_stocks_consts x y q w subreddit_list

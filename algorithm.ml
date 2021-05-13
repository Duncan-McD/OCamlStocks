let do_thing (h : Parser.post) x y =
  if Parser.connotation h < 0. then
    -1.
    *. Float.pow (float_of_int (Parser.upvote_score h)) x
    *. Float.pow (-1. *. Parser.connotation h) (1. /. y)
  else
    Float.pow (float_of_int (Parser.upvote_score h)) x
    *. Float.pow (Parser.connotation h) (1. /. y)

let rec process_posts (post_list : Parser.post list) x y acc =
  match post_list with
  | [] -> acc
  | h :: t -> process_posts t x y (acc +. do_thing h x y)

let get_data list_of_stocks x y q w stock_name =
  let data = Parser.data list_of_stocks stock_name in
  let post_list = snd data in
  let num_posts = float_of_int (List.length post_list) in
  (process_posts post_list x y 0. /. num_posts)
  +. (q *. num_posts)
  +. (w *. fst data)

let rec process (list_of_stocks : string list)
    (parsed_subreddit : Parser.stocks) x y q w (acc : float list) =
  match list_of_stocks with
  | [] -> acc
  | h :: t ->
      process t parsed_subreddit x y q w
        (get_data parsed_subreddit x y q w h :: acc)

let get_processed_stock_list x y q w parsed_subreddit =
  let list_of_stocks = Parser.stock_names parsed_subreddit in
  List.combine list_of_stocks
    (process list_of_stocks parsed_subreddit x y q w [])

let is_snd_neg (a, b) = b < 0.

let is_snd_pos (a, b) = b > 0.

let get_stocks_to_sell_list x y q w subreddit =
  List.filter is_snd_neg (get_processed_stock_list x y q w subreddit)

let rec sum_snd pos_list acc =
  match pos_list with [] -> acc | (a, b) :: t -> sum_snd t (acc +. b)

let rec get_weighted_list sum_of_list list acc =
  match list with
  | [] -> acc
  | (a, b) :: t -> get_weighted_list sum_of_list t ((a, b /. sum_of_list) :: acc)

let get_stocks_to_buy_list x y q w subreddit =
  let pos_list =
    List.filter is_snd_pos (get_processed_stock_list x y q w subreddit)
  in
  let sum = sum_snd pos_list 0. in
  get_weighted_list sum pos_list []

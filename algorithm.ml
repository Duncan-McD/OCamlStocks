let do_thing (h : Parser.post) x y =
  Float.pow (float_of_int (Parser.upvote_score h)) x
  *. Float.pow (Parser.connotation h) (1. /. y)

let rec process_posts (post_list : Parser.post list) x y acc =
  match post_list with
  | [] -> acc
  | h :: t -> process_posts t x y (acc +. do_thing h x y)

let get_data list_of_stocks x y q w stock_name =
  let post_list = Parser.data list_of_stocks stock_name in
  let num_posts = float_of_int (List.length post_list) in
  (process_posts post_list x y 0. /. num_posts)
  +. (q *. num_posts)
  +. (w *. 0. (*TODO: change 0 to Parser.history_score*))

let rec process (list_of_stocks : string list)
    (parsed_subreddit : Parser.stocks) x y q w (acc : float list) =
  match list_of_stocks with
  | [] -> acc
  | h :: t ->
      process t parsed_subreddit x y q w
        (get_data parsed_subreddit x y q w h :: acc)

let get_processed_stock_list x y q w subreddit =
  let parsed_subreddit = Parser.parse subreddit in
  let list_of_stocks = Parser.stock_names parsed_subreddit in
  List.combine list_of_stocks
    (process list_of_stocks parsed_subreddit x y q w [])

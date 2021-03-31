type stock_data = {
  upvote_ratio : float;
  upvote_total : int;
  connotation : int;
}

type stock = { name : string; data : stock_data list }

type post = {
  title : string;
  body : string;
  upvote_ratio : float;
  upvote_total : int;
}

(*  [parse_post post_text parsed_data] is an [(n * d) list] obtained by parsing the post where 
    [n] is the name of a mentioned stock and [d] is the stock_data *)
let rec parse_post post_text stock_data parsed_data =
  match post_text with
  | [] -> parsed_data
  | word :: remaining_words ->
      if Cashset.is_stock_name word && List.mem_assoc word parsed_data = false
      then
        parse_post remaining_words stock_data ((word, stock_data) :: parsed_data)
      else parse_post remaining_words stock_data parsed_data

(* [get_stock_data posts] is the [stock_data list] obtained by parsing [posts] *)
let rec get_stocks_data posts stocks_data =
  match posts with
  | [] -> stocks_data
  | post :: remaining_posts ->
      let uv_ratio = Scraper.upvote_ratio post in
      (* score isnt total change later *)
      let uv_total = Scraper.score post in
      (* connotation function TBD *)
      let con = 0 in
      let stock_data =
        { upvote_ratio = uv_ratio; upvote_total = uv_total; connotation = con }
      in
      let post_title_body = Scraper.title post ^ Scraper.body post in
      let post_text = post_title_body |> String.split_on_char ' ' in
      let unfiltered_stocks_data = parse_post post_text stock_data [] in
      unfiltered_stocks_data

let parse subreddit = Scraper.posts subreddit |> get_stocks_data

let name stock = stock.name

let data stock = stock.data

let score post = Scraper.score

let ratio post = Scraper.upvote_ratio

let connotation post = failwith "Unimplemented"

type post = { score : int; upvote_ratio : float; connotation : int }

type stocks = (string, post list) Hashtbl.t

let connotation post = 0

(*  [update_one_stock stock_name post stocks] is [stocks] with the new post data 
    added to the stock [stock_name] *)
let update_one_stock stock_name post stocks =
  if Hashtbl.mem stocks stock_name then
    let posts = Hashtbl.find stocks stock_name in
    Hashtbl.replace stocks stock_name (post :: posts)
  else Hashtbl.add stocks stock_name [ post ];
  stocks

(*  [update_stocks text post stocks_seen stocks] is [stocks] 
    updated by reading each word in [text] and, if the word is a stock,
    updating that stock with the post data [post] 
    
    [stocks_seen] keeps track of each stock read in the post to avoid 
    adding [post] to the same stock more than once *)
let rec update_stocks text post stocks_seen stocks =
  match text with
  | [] -> stocks
  | w :: t ->
      if Cashset.is_stock_name w && List.exists (fun p -> p = w) stocks_seen
      then
        let stocks' = update_one_stock w post stocks in
        update_stocks t post (w :: stocks_seen) stocks'
      else update_stocks t post stocks_seen stocks

(* TODO: stocks $GME and GME will be put treated as separate stocks with their own post data fix *)
(* [populate_stocks] is the [stocks] hashtable created from the data in [posts] *)
let rec populate_stocks posts stocks =
  match posts with
  | [] -> stocks
  | p :: t ->
      let post =
        {
          score = Scraper.score p;
          upvote_ratio = Scraper.upvote_ratio p;
          connotation = connotation p;
        }
      in
      let title = Scraper.title p in
      let body = Scraper.body p in
      let text = title ^ " " ^ body |> String.split_on_char ' ' in
      let stocks' = update_stocks text post [] stocks in
      populate_stocks t stocks'

let parse subreddit =
  let stocks = Hashtbl.create 500 in
  let posts = Scraper.posts subreddit in
  populate_stocks posts stocks

let stock_names stocks =
  Hashtbl.fold (fun name posts stock_names -> name :: stock_names) stocks []

let data stocks stock_name = Hashtbl.find

let upvote_score post = post.score

let upvote_ratio post = post.upvote_ratio

let connotation post = post.connotation

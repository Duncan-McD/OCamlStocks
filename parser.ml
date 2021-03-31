type post = { score : int; upvote_ratio : float; connotation : int }

type stocks = (string, post list) Hashtbl.t

(* [populate_stocks] is the [stocks] hashtable created from the data in [posts] *)
let rec populate_stocks posts stocks = stocks

let parse subreddit =
  let stocks = Hashtbl.create 500 in
  let posts = Scraper.posts subreddit in
  populate_stocks posts stocks

let name stocks =
  Hashtbl.fold (fun name posts stock_names -> name :: stock_names) stocks []

let data stocks stock_name = Hashtbl.find

let score post = post.upvote_ratio

let upvote_ratio post = post.upvote_ratio

let connotation post = post.connotation

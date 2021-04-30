type post = { score : int; upvote_ratio : float; connotation : float }

type stocks = (string, float * post list) Hashtbl.t

let connotation_str str = 
  Py.initialize ();
  let vader = Py.import "vaderSentiment.vaderSentiment" in
  let sentAnalyzer = Py.Module.get_function vader "SentimentIntensityAnalyzer" [||] in
  let polarityScore = Py.Module.get_function sentAnalyzer "polarity_scores" in
  let resultDict = polarityScore [| Py.String.of_string str |] in
  let compound = Py.Dict.get_item resultDict (Py.String.of_string "compound") in
  let result = match compound with
  | None -> failwith "impossible"
  | Some x -> Py.Float.to_float x in
  Py.finalize ();
  result

let connotation_post post = 
  let postText = Scraper.title post ^ "\n" ^ Scraper.body post in
  connotation_str postText

let history_score stock_data =
  let rating =
    match stock_data with Some sd -> Stockdata.rating sd | None -> 3.
  in
  -1. *. tan (Float.pi /. 4. *. (rating -. 3.))

let convert_to_ticker s =
  if String.get s 0 = '$' then String.sub s 1 (String.length s - 1) else s

(*  [update_one_stock stock_name post stocks] is [stocks] with the new post data 
    added to the stock [stock_name] *)
let update_one_stock stock_name post stocks =
  if Hashtbl.mem stocks stock_name then
    let data = Hashtbl.find stocks stock_name in
    match data with h, p -> Hashtbl.replace stocks stock_name (h, post :: p)
  else
    Hashtbl.add stocks stock_name
      (history_score (stock_name |> Stockdata.stockdata_from_ticker), [ post ]);
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
      if
        Cashset.is_stock_name w
        && Bool.not (List.exists (fun p -> p = convert_to_ticker w) stocks_seen)
      then
        let stocks' = update_one_stock (convert_to_ticker w) post stocks in
        update_stocks t post (convert_to_ticker w :: stocks_seen) stocks'
      else update_stocks t post stocks_seen stocks

(* [populate_stocks] is the [stocks] hashtable created from the data in [posts] *)
let rec populate_stocks posts stocks =
  match posts with
  | [] -> stocks
  | p :: t ->
      let post =
        {
          score = Scraper.score p;
          upvote_ratio = Scraper.upvote_ratio p;
          connotation = connotation_post p;
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

let data = Hashtbl.find

let upvote_score post = post.score

let upvote_ratio post = post.upvote_ratio

let connotation post = post.connotation
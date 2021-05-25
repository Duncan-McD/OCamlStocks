open Mechaml
module M = Agent.Monad
open M.Infix
open Soup

exception StockNotFound of string

(** [get_soup site] is the soup node representation of website [site] *)
let get_soup (site : string) : soup node M.m =
  Agent.get site >|= Agent.HttpResponse.page >|= Page.soup

(** [get_full_page_json soup_node] is the json representation of [soup_node] *)
let get_one_tag (tag : string) (soup_node : soup node M.m) : string =
  match M.run (Agent.init ()) soup_node with
  | _, node ->
      node |> (*select_one ".YDC-Lead" |> require |> *) select_one tag
      (* |> Req.leaf_text *)
      |> require
      |> leaf_text |> require

exception ExitLoop of int

(**[remove_commas o s] is the string [s] without commas*)
let rec remove_commas o s =
  if String.length s <> 0 then
    if s.[0] = ',' then remove_commas o (String.sub s 1 (String.length s - 1))
    else
      remove_commas
        (o ^ Char.escaped s.[0])
        (String.sub s 1 (String.length s - 1))
  else o

(**[index_of_substring s sub] the int corresponding to the index of [s] 
that is the first instance of [sub]*)
let index_of_substring (s : string) (sub : string) : int =
  try
    let sub_length = String.length sub in
    for i = 0 to String.length s - sub_length do
      if String.sub s i sub_length = sub then raise (ExitLoop i)
    done;
    -1
  with ExitLoop i -> i

(** [change_selector] is the css selector for stock day change *)
let change_selector =
  "#quote-header-info > div.My\\(6px\\).Pos\\(r\\).smartphone_Mt\\(6px\\) > \
   div.D\\(ib\\).Va\\(m\\).Maw\\(65\\%\\).Ov\\(h\\) > div > \
   span.Trsdu\\(0\\.3s\\).Fw\\(500\\).Pstart\\(10px\\).Fz\\(24px\\)"

(* [value_selector] is the css selector for stock value *)
let value_selector =
  "#quote-header-info > div.My\\(6px\\).Pos\\(r\\).smartphone_Mt\\(6px\\) > \
   div.D\\(ib\\).Va\\(m\\).Maw\\(65\\%\\).Ov\\(h\\) > div > \
   span.Trsdu\\(0\\.3s\\).Fw\\(b\\).Fz\\(36px\\).Mb\\(-4px\\).D\\(ib\\)"

(* [rating_selector] is the css selector for rating *)
let rating_selector = "body > div + div + script"

(* [does_not_exist_selector] is the css selector to indicate stock existence *)
let does_not_exist_selector = "#lookup-page > section > div > h2 > span"

(* [does_not_exist_selector2] is a secondary css selector to indicate 
stock existence *)
let does_not_exist_selector2 =
  "#quote-summary > \
   div.D\\(ib\\).W\\(1\\/2\\).Bxz\\(bb\\).Pend\\(12px\\).Va\\(t\\).ie-7_D\\(i\\).smartphone_D\\(b\\).smartphone_W\\(100\\%\\).smartphone_Pend\\(0px\\).smartphone_BdY.smartphone_Bdc\\(\\$seperatorColor\\) \
   > table > tbody > tr:nth-child(1) > td.Ta\\(end\\).Fw\\(600\\).Lh\\(14px\\) \
   > span"

(** [scrape_value l] is the current value of the stock in the link [l] *)
let scrape_value link =
  let value =
    link |> get_soup |> get_one_tag value_selector |> remove_commas ""
  in
  float_of_string value

(** [scrape_value l] is the current change of the stock in the link [l] 
    since open *)
let scrape_change link =
  let change = get_one_tag change_selector (get_soup link) in
  let change_end = String.index change '(' in
  let new_change = String.sub change 0 (change_end - 1) in
  let value = new_change |> remove_commas "" in
  float_of_string value

(** [scrape_rating l] is the current rating of stock from link [l] *)
let scrape_rating link =
  try
    let rating_script = get_one_tag rating_selector (get_soup link) in
    let recommendation_key = "\"recommendationMean\":{\"raw\":" in
    let recommendation_value_location =
      index_of_substring rating_script recommendation_key
      + String.length recommendation_key
    in
    let remaining_things =
      String.sub rating_script recommendation_value_location
        (String.length rating_script - recommendation_value_location)
    in
    let formatted_key = "\"fmt\":\"" in
    let formatted_value_location =
      index_of_substring remaining_things formatted_key
      + String.length formatted_key
    in
    let recommendation_value =
      String.sub remaining_things formatted_value_location 3
    in
    let value = recommendation_value |> remove_commas "" in
    float_of_string value
  with e -> 3.

type t = { value : float; change : float; ticker : string; rating : float }

(** [yahoo_finance_link t] returns the link to the page on Yahoo Finance for 
    ticker [t] *)
let yahoo_finance_link ticker = "https://finance.yahoo.com/quote/" ^ ticker

(** [contains s sub] is true if [sub] is a substring in [s] *)
let contains s sub =
  try
    let sub_length = String.length sub in
    for i = 0 to String.length s - sub_length do
      if String.sub s i sub_length = sub then raise Exit
    done;
    false
  with Exit -> true

(** [stock_exists t] is true if the stock with ticker [t] is on Yahoo Finance *)
let stock_exists ticker =
  let link = yahoo_finance_link ticker in
  try
    let _ = get_one_tag value_selector (get_soup link) in
    true
    && Cashset.is_stock_name ticker
    && get_one_tag does_not_exist_selector2 (get_soup link) <> "N/A"
  with Failure s -> false

(**[stockdata_from_ticker t] is the stockdata of ticker [t]. Returns None 
if the stock does not exist*)
let stockdata_from_ticker (ticker : string) : t option =
  if stock_exists ticker = false then None
  else
    Some
      {
        value = scrape_value (yahoo_finance_link ticker);
        change = scrape_change (yahoo_finance_link ticker);
        ticker;
        rating = scrape_rating (yahoo_finance_link ticker);
      }

let value (stock_data : t) : float = stock_data.value

let change (stock_data : t) : float = stock_data.change

let ticker (stock_data : t) : string = stock_data.ticker

let rating (stock_data : t) : float = stock_data.rating

let require ticker (stock_data : t option) : t =
  match stock_data with None -> raise (StockNotFound ticker) | Some sd -> sd

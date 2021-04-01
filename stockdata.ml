open Mechaml
module M = Agent.Monad
open M.Infix
open Soup

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

(* css selector for stock day change *)
let change_selector =
  "#quote-header-info > div.My\(6px\).Pos\(r\).smartphone_Mt\(6px\) > \
   div.D\(ib\).Va\(m\).Maw\(65\%\).Ov\(h\) > div > \
   span.Trsdu\(0\.3s\).Fw\(500\).Pstart\(10px\).Fz\(24px\)"

(* css selector for stock value *)
let value_selector =
  "#quote-header-info > div.My\(6px\).Pos\(r\).smartphone_Mt\(6px\) > \
   div.D\(ib\).Va\(m\).Maw\(65\%\).Ov\(h\) > div > \
   span.Trsdu\(0\.3s\).Fw\(b\).Fz\(36px\).Mb\(-4px\).D\(ib\)"

(** [scrape_value l] is the current value of the stock in the link [l] *)
let scrape_value link =
  float_of_string (get_one_tag value_selector (get_soup link))

(** [scrape_value l] is the current change of the stock in the link [l] 
    since open *)
let scrape_change link =
  let change = get_one_tag change_selector (get_soup link) in
  let change_end = String.index change '(' in
  let new_change = String.sub change 0 (change_end - 1) in
  float_of_string new_change

type t = { value : float; change : float }

(** [yahoo_finance_link t] returns the link to the page on Yahoo Finance for 
    ticker [t] *)
let yahoo_finance_link ticker = "https://finance.yahoo.com/quote/" ^ ticker

let stockdata_from_ticker (ticker : string) : t =
  {
    value = scrape_value (yahoo_finance_link ticker);
    change = scrape_change (yahoo_finance_link ticker);
  }

let value (stock_data : t) : float = stock_data.value

let change (stock_data : t) : float = stock_data.value

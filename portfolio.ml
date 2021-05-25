open Yojson.Basic
open Yojson.Basic.Util

type stock = {
  ticker : string;
  shares : float;
  price_per_share : float;
  initial_value : float;
  value : float;
  stock_change : float;
}

type t = {
  liquidity : float;
  stocks : (string, stock) Hashtbl.t;
  net_worth : float;
  change : float;
  first : bool;
  vars : float * float * float * float;
  timestamp : float;
}

(**[current_cost t] is the current cost of ticker [t]*)
let current_cost ticker =
  ticker |> Stockdata.stockdata_from_ticker |> Stockdata.require ticker
  |> Stockdata.value

let portfolio_gain_loss (portfolio : t) : float = portfolio.change

let net_worth (portfolio : t) : float = portfolio.net_worth

let liquidity (portfolio : t) : float = portfolio.liquidity

let shares (stock : stock) : float = stock.shares

let ticker (stock : stock) : string = stock.ticker

let stock_gain_loss (stock : stock) : float = stock.stock_change

let empty_portfolio =
  {
    liquidity = 0.;
    stocks = Hashtbl.create 50;
    net_worth = 0.;
    change = 0.;
    first = false;
    vars = (0., 0., 0., 0.);
    timestamp = Unix.time ();
  }

let list_of_tickers (portfolio : t) : string list =
  Hashtbl.fold
    (fun name stock stock_names -> name :: stock_names)
    portfolio.stocks []

let list_of_shares (portfolio : t) =
  Hashtbl.fold
    (fun name stock stocks -> stock.shares :: stocks)
    portfolio.stocks []

let list_of_ppss (portfolio : t) =
  Hashtbl.fold
    (fun name stock stocks -> stock.price_per_share :: stocks)
    portfolio.stocks []

let list_of_values (portfolio : t) =
  Hashtbl.fold
    (fun name stock stocks -> stock.value :: stocks)
    portfolio.stocks []

let list_of_changes (portfolio : t) =
  Hashtbl.fold
    (fun name stock stocks -> stock.stock_change :: stocks)
    portfolio.stocks []

let list_of_stocks (portfolio : t) : stock list =
  Hashtbl.fold (fun name stock stocks -> stock :: stocks) portfolio.stocks []

let stock_from_ticker (portfolio : t) (ticker : string) : stock option =
  if Bool.not (Hashtbl.mem portfolio.stocks ticker) then None
  else Some (Hashtbl.find portfolio.stocks ticker)

(**[buy_shares p t s c] is the portfolio after purchasing [s] shares of the
 stock of stock ticker [t] at a cost of [c]*)

(*TODO: Break function*)
let buy_shares portfolio ticker shares cost =
  let liquidity = portfolio.liquidity in
  let cost_per_share = cost in
  let diff = (cost_per_share *. shares) -. liquidity in
  let new_liquidity = if diff > 0. then 0. else -1. *. diff in
  let new_shares = if diff > 0. then liquidity /. cost_per_share else shares in
  let stock_exists = Hashtbl.mem portfolio.stocks ticker in
  if stock_exists then (
    let past_stock = Hashtbl.find portfolio.stocks ticker in
    let new_shares = past_stock.shares +. new_shares in
    let value = cost_per_share *. new_shares in
    let recent_change = value -. past_stock.value in
    let new_stock =
      {
        ticker;
        shares = new_shares;
        price_per_share = cost_per_share;
        initial_value = past_stock.initial_value;
        value;
        stock_change = recent_change;
      }
    in
    Hashtbl.remove portfolio.stocks ticker;
    Hashtbl.add portfolio.stocks ticker new_stock;
    let new_portfolio =
      {
        liquidity = new_liquidity;
        stocks = portfolio.stocks;
        net_worth = portfolio.net_worth +. recent_change;
        change =
          (if portfolio.first then 0. +. recent_change
          else portfolio.change +. recent_change);
        first = false;
        vars = portfolio.vars;
        timestamp = Unix.time ();
      }
    in
    new_portfolio)
  else
    let initial_value = cost_per_share *. new_shares in
    let recent_change = initial_value in
    let value = initial_value in
    let new_stock =
      {
        ticker;
        shares = new_shares;
        price_per_share = cost_per_share;
        initial_value;
        value;
        stock_change = recent_change;
      }
    in
    Hashtbl.add portfolio.stocks ticker new_stock;
    let new_portfolio =
      {
        liquidity = new_liquidity;
        stocks = portfolio.stocks;
        net_worth = portfolio.net_worth +. recent_change;
        change =
          (if portfolio.first then 0. +. recent_change
          else portfolio.change +. recent_change);
        first = false;
        vars = portfolio.vars;
        timestamp = Unix.time ();
      }
    in
    new_portfolio

(*TODO: Break function*)

(**[sell_shares p t s c] is the portfolio after selling [s] shares of the
 stock of stock ticker [t] at a cost of [c]*)
let sell_shares portfolio ticker shares cost =
  let liquidity = portfolio.liquidity in
  let cost_per_share = cost in
  let stock_exists = Hashtbl.mem portfolio.stocks ticker in
  if stock_exists then (
    let past_stock = Hashtbl.find portfolio.stocks ticker in
    let past_val = past_stock.value in
    let diff = (cost_per_share *. shares) -. past_val in
    let new_shares = if diff > 0. then past_val /. cost_per_share else shares in
    let new_liquidity = liquidity +. (new_shares *. cost_per_share) in
    let new_shares = shares -. new_shares in
    let value = cost_per_share *. new_shares in
    let recent_change = value -. past_stock.value in
    let new_stock =
      {
        ticker;
        shares = new_shares;
        price_per_share = cost_per_share;
        initial_value = past_stock.initial_value;
        value;
        stock_change = recent_change;
      }
    in
    Hashtbl.remove portfolio.stocks ticker;
    if value > 0. then Hashtbl.add portfolio.stocks ticker new_stock else ();

    let new_portfolio =
      {
        liquidity = new_liquidity;
        stocks = portfolio.stocks;
        net_worth = portfolio.net_worth +. recent_change;
        change =
          (if portfolio.first then 0. +. recent_change
          else portfolio.change +. recent_change);
        first = false;
        vars = portfolio.vars;
        timestamp = Unix.time ();
      }
    in

    new_portfolio)
  else
    let new_portfolio =
      {
        liquidity = portfolio.liquidity;
        stocks = portfolio.stocks;
        net_worth = portfolio.net_worth;
        change = (if portfolio.first then 0. else portfolio.change);
        first = false;
        vars = portfolio.vars;
        timestamp = Unix.time ();
      }
    in
    new_portfolio

(**[change_ticker_shares p t s c] is the changing the amount [s] shares of the
 stock of stock ticker [t] at a cost of [c]*)
let change_ticker_shares (portfolio : t) (ticker : string) (shares : float)
    (cost : float) : t =
  if shares > 0. then buy_shares portfolio ticker shares cost
  else if shares < 0. then sell_shares portfolio ticker (-1. *. shares) cost
  else buy_shares portfolio ticker 0. cost

(**[change_ticker_money p t s c] is the change [s] shares of the
 stock of stock ticker [t] at a cost of [c] to a new ticker money*)
let change_ticker_money (portfolio : t) (ticker : string) (money : float) : t =
  let cost_per_share = current_cost ticker in
  let shares = money /. cost_per_share in
  change_ticker_shares portfolio ticker shares cost_per_share

(**[change_vars p (x,y,w1,w2)] is the portfolio whose vars are now (x,y,w1,w2)*)
let change_vars (portfolio : t) (x, y, w1, w2) =
  {
    liquidity = portfolio.liquidity;
    stocks = portfolio.stocks;
    net_worth = portfolio.net_worth;
    change = portfolio.change;
    first = Bool.not portfolio.first;
    vars = (x, y, w1, w2);
    timestamp = Unix.time ();
  }

(**[portfolio_swap_first p] is the portfolio whose first is the opposite of [p]*)
let portfolio_swap_first portfolio =
  {
    liquidity = portfolio.liquidity;
    stocks = portfolio.stocks;
    net_worth = portfolio.net_worth;
    change = portfolio.change;
    first = Bool.not portfolio.first;
    vars = portfolio.vars;
    timestamp = Unix.time ();
  }

(**[refresh_stock p t] is the portfolio with the stock of ticker [t] 
updated with its current worth*)
let refresh_stock portfolio ticker = change_ticker_money portfolio ticker 0.

(**[rec_refresh_portfolio t p] is the portfolio with each stock 
updated with its current worth*)
let rec rec_refresh_portfolio ticker_names portfolio =
  match ticker_names with
  | [] -> portfolio
  | h :: t -> rec_refresh_portfolio t (refresh_stock portfolio h)

let copy portfolio =
  {
    liquidity = portfolio.liquidity;
    stocks = Hashtbl.copy portfolio.stocks;
    net_worth = portfolio.net_worth;
    change = portfolio.change;
    first = portfolio.first;
    vars = portfolio.vars;
    timestamp = Unix.time ();
  }

let refresh portfolio =
  portfolio |> copy |> portfolio_swap_first
  |> rec_refresh_portfolio (list_of_tickers portfolio)

(**[rec_sell t p] is the portfolio with each stock sold*)
let rec rec_sell ticker_names portfolio =
  match ticker_names with
  | [] -> portfolio
  | h :: t ->
      rec_sell t
        (let needed_stock = stock_from_ticker portfolio h in
         match needed_stock with
         | None -> portfolio
         | Some s ->
             change_ticker_shares portfolio h (-1. *. s.shares) (current_cost h))

(**[sell_stocks p s] is the portfolio with each stock sold*)
let sell_stocks portfolio stocks =
  portfolio |> portfolio_swap_first |> rec_sell stocks

(**[rec_buy t p l] is the portfolio with each stock in [t] bought*)
let rec rec_buy ticker_names portfolio liquidity =
  match ticker_names with
  | [] -> if portfolio.first then portfolio_swap_first portfolio else portfolio
  | (s, f) :: t ->
      rec_buy t (change_ticker_money portfolio s (liquidity *. f)) liquidity

(**[buy_stocks p s] is the portfolio with each stock in [s] bought*)
let buy_stocks portfolio stocks =
  let initial_liquidity = portfolio.liquidity in
  rec_buy stocks portfolio initial_liquidity

let process portfolio (x, y, w1, w2) = function
  | buy, sell ->
      let portfolio = copy portfolio in
      let post_sell_portfolio = sell_stocks portfolio sell in
      change_vars (buy_stocks post_sell_portfolio buy) (x, y, w1, w2)

let compare portfolio1 portfolio2 =
  if portfolio1.net_worth > portfolio2.net_worth then 1
  else if portfolio1.net_worth < portfolio2.net_worth then -1
  else 0

let change_liquidity portfolio liquid =
  let new_portfolio = refresh portfolio in
  {
    liquidity = new_portfolio.liquidity +. liquid;
    stocks = Hashtbl.copy new_portfolio.stocks;
    net_worth = new_portfolio.net_worth;
    change = new_portfolio.change;
    first = new_portfolio.first;
    vars = new_portfolio.vars;
    timestamp = new_portfolio.timestamp;
  }

let vars portfolio = portfolio.vars

(**[stock_to_json s] is the json of stock [s]*)
let stock_to_json (stock : stock) =
  `Assoc
    [
      ("ticker", `String stock.ticker);
      ("shares", `Float stock.shares);
      ("price_per_share", `Float stock.price_per_share);
      ("initial_value", `Float stock.initial_value);
      ("value", `Float stock.value);
      ("change", `Float stock.stock_change);
    ]

(**[string_of_stocklist s] is the string in json format of stocklist [s]*)

(**[fst4 t] is the first element in 4-tuple [t]*)
let fst4 (quad : float * float * float * float) =
  match quad with a, b, c, d -> a

(**[snd4 t] is the second element in 4-tuple [t]*)
let snd4 (quad : float * float * float * float) =
  match quad with a, b, c, d -> b

(**[trd4 t] is the third element in 4-tuple [t]*)
let trd4 (quad : float * float * float * float) =
  match quad with a, b, c, d -> c

(**[fth4 t] is the fourth element in 4-tuple [t]*)
let fth4 (quad : float * float * float * float) =
  match quad with a, b, c, d -> d

(**[vars_to_json t] is json of vars [t]*)
let vars_to_json (portfolio : t) =
  `Assoc
    [
      ("x", `Float (fst4 portfolio.vars));
      ("y", `Float (snd4 portfolio.vars));
      ("w1", `Float (trd4 portfolio.vars));
      ("w2", `Float (fth4 portfolio.vars));
    ]

(**[stock_list_to_json t] is json of stock list [t]*)
let rec stock_list_to_json (stock_list : stock list) acc =
  match stock_list with
  | [] -> `List acc
  | h :: t -> stock_list_to_json t (stock_to_json h :: acc)

let to_json t =
  `Assoc
    [
      ("liquidity", `Float t.liquidity);
      ("stocks", stock_list_to_json (list_of_stocks t) []);
      ("net_worth", `Float t.net_worth);
      ("change", `Float t.change);
      ("first", `Bool t.first);
      ("vars", vars_to_json t);
      ("timestamp", `Float t.timestamp);
    ]

(**[stock_of_json j] is the stock representation of json [j]*)
let stock_of_json j =
  {
    ticker = to_string (member "ticker" j);
    shares = to_float (member "shares" j);
    price_per_share = to_float (member "price_per_share" j);
    initial_value = to_float (member "initial_value" j);
    value = to_float (member "value" j);
    stock_change = to_float (member "change" j);
  }

(**[stock_name_of_json j] is the ticker of the stock of json [j] *)
let stock_name_of_json j = to_string (member "ticker" j)

(**[stocks_of_json_stocklist j l htbl] is  representation of json [j] *)
let rec stocks_of_json_stocklist (j : Yojson.Basic.t) list_of_stocks htbl =
  match list_of_stocks with
  | [] -> htbl
  | h :: t ->
      stocks_of_json_stocklist j t
        (Hashtbl.add htbl (stock_name_of_json h) (stock_of_json h);
         htbl)

(**[stocks_of_json j] is the stocks representation of json [j]*)
let stocks_of_json (j : Yojson.Basic.t) =
  let list_of_stocks = to_list (member "stocks" j) in
  stocks_of_json_stocklist j list_of_stocks (Hashtbl.create 50)

(**[stocks_of_json j] is the vars representation of json [j]*)
let vars_of_json (j : Yojson.Basic.t) =
  ( to_float (member "x" j),
    to_float (member "y" j),
    to_float (member "w1" j),
    to_float (member "w2" j) )

let portfolio_of_json (j : Yojson.Basic.t) =
  {
    liquidity = to_float (member "liquidity" j);
    stocks = stocks_of_json j;
    net_worth = to_float (member "net_worth" j);
    change = to_float (member "change" j);
    first = to_bool (member "first" j);
    vars = vars_of_json (member "vars" j);
    timestamp = to_float (member "timestamp" j);
  }

let sell_all portfolio =
  let new_portfolio = refresh portfolio in
  {
    liquidity = new_portfolio.liquidity +. new_portfolio.net_worth;
    stocks = Hashtbl.create 50;
    net_worth = 0.;
    change = -1. *. new_portfolio.net_worth;
    first = false;
    vars = (1., 1., 1., 1.);
    timestamp = Unix.time ();
  }

let value stock = stock.value

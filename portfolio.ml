type stock = {
  ticker : string;
  shares : float;
  price_per_share : float;
  initial_value : float;
  value : float;
  change : float;
}

type t = {
  liquidity : float;
  stocks : (string, stock) Hashtbl.t;
  net_worth : float;
  change : float;
  first : bool;
  timestamp : float;
}

let current_cost ticker =
  ticker |> Stockdata.stockdata_from_ticker |> Stockdata.require ticker
  |> Stockdata.value

let portfolio_gain_loss (portfolio : t) : float = portfolio.change

let net_worth (portfolio : t) : float = portfolio.net_worth

let liquidity (portfolio : t) : float = portfolio.liquidity

let shares (stock : stock) : float = stock.shares

let ticker (stock : stock) : string = stock.ticker

let stock_gain_loss (stock : stock) : float = stock.change

let empty_portfolio =
  {
    liquidity = 0.;
    stocks = Hashtbl.create 50;
    net_worth = 0.;
    change = 0.;
    first = false;
    timestamp = Unix.time ();
  }

let list_of_tickers (portfolio : t) : string list =
  Hashtbl.fold
    (fun name stock stock_names -> name :: stock_names)
    portfolio.stocks []

let list_of_stocks (portfolio : t) : stock list =
  Hashtbl.fold (fun name stock stocks -> stock :: stocks) portfolio.stocks []

let stock_from_ticker (portfolio : t) (ticker : string) : stock option =
  if Bool.not (Hashtbl.mem portfolio.stocks ticker) then None
  else Some (Hashtbl.find portfolio.stocks ticker)

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
        change = recent_change;
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
        change = recent_change;
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
        timestamp = Unix.time ();
      }
    in
    new_portfolio

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
        change = recent_change;
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
        timestamp = Unix.time ();
      }
    in
    new_portfolio

let change_ticker_shares (portfolio : t) (ticker : string) (shares : float)
    (cost : float) : t =
  if shares > 0. then buy_shares portfolio ticker shares cost
  else if shares < 0. then sell_shares portfolio ticker (-1. *. shares) cost
  else buy_shares portfolio ticker 0. cost

let change_ticker_money (portfolio : t) (ticker : string) (money : float) : t =
  let cost_per_share = current_cost ticker in
  let shares = money /. cost_per_share in
  change_ticker_shares portfolio ticker shares cost_per_share

let portfolio_swap_first portfolio =
  {
    liquidity = portfolio.liquidity;
    stocks = portfolio.stocks;
    net_worth = portfolio.net_worth;
    change = portfolio.change;
    first = Bool.not portfolio.first;
    timestamp = Unix.time ();
  }

let refresh_stock portfolio ticker = change_ticker_money portfolio ticker 0.

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
    timestamp = Unix.time ();
  }

let refresh portfolio =
  portfolio |> copy |> portfolio_swap_first
  |> rec_refresh_portfolio (list_of_tickers portfolio)

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

let sell_stocks portfolio stocks =
  portfolio |> portfolio_swap_first |> rec_sell stocks

let rec rec_buy ticker_names portfolio liquidity =
  match ticker_names with
  | [] -> if portfolio.first then portfolio_swap_first portfolio else portfolio
  | (s, f) :: t ->
      rec_buy t (change_ticker_money portfolio s (liquidity *. f)) liquidity

let buy_stocks portfolio stocks =
  let initial_liquidity = portfolio.liquidity in
  rec_buy stocks portfolio initial_liquidity

let process portfolio = function
  | buy, sell ->
      let portfolio = copy portfolio in
      let post_sell_portfolio = sell_stocks portfolio sell in
      buy_stocks post_sell_portfolio buy

let compare portfolio1 portfolio2 =
  if portfolio1.net_worth > portfolio2.net_worth then 1.
  else if portfolio1.net_worth < portfolio2.net_worth then -1.
  else 0.

let change_liquidity portfolio liquid =
  {
    liquidity = portfolio.liquidity +. liquid;
    stocks = Hashtbl.copy portfolio.stocks;
    net_worth = portfolio.net_worth;
    change = portfolio.change;
    first = portfolio.first;
    timestamp = Unix.time ();
  }

let sell_all portfolio =
  let new_portfolio = refresh portfolio in
  {
    liquidity = new_portfolio.liquidity +. new_portfolio.net_worth;
    stocks = Hashtbl.create 50;
    net_worth = 0.;
    change = -1. *. new_portfolio.net_worth;
    first = false;
    timestamp = Unix.time ();
  }

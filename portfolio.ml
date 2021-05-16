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
}

let current_cost ticker =
  ticker |> Stockdata.stockdata_from_ticker |> Stockdata.require
  |> Stockdata.value

let portfolio_gain_loss (portfolio : t) : float = portfolio.change

let get_net_worth (portfolio : t) : float = portfolio.net_worth

let get_liquidity (portfolio : t) : float = portfolio.liquidity

let get_shares (stock : stock) : float = stock.shares

let get_ticker (stock : stock) : string = stock.ticker

let stock_gain_loss (stock : stock) : float = stock.change

let list_of_tickers (portfolio : t) : string list =
  Hashtbl.fold (fun name stocks stock_names -> name :: stock_names) stocks []

let list_of_stocks (portfolio : t) : stock list = failwith "unimplemented"

let stock_from_ticker (portfolio : t) (ticker : string) : stock option =
  if Hashtbl.mem portfolio.stocks ticker then None
  else Some (Hashtbl.find portfolio.stocks ticker)

let buy_shares portfolio ticker shares =
  let liquidity = portfolio.liquidity in
  let cost_per_share = current_cost ticker in
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
      }
    in
    new_portfolio)
  else
    let recent_change = 0. in
    let initial_value = cost_per_share *. new_shares in
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
      }
    in
    new_portfolio

let sell_shares portfolio ticker shares =
  let liquidity = portfolio.liquidity in
  let cost_per_share = current_cost ticker in
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
      }
    in
    new_portfolio)
  else
    let recent_change = 0. in
    let initial_value = cost_per_share *. new_shares in
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
      }
    in
    new_portfolio

let change_ticker_shares (portfolio : t) (ticker : string) (shares : float)
    (liquidity : float) : t =
  if shares > 0. then buy_shares portfolio ticker shares
  else if shares < 0. then sell_shares ()
  else portfolio

let change_stock_shares (portfolio : t) (stock : stock) (shares : int)
    (liquidity : float) : t =
  failwith "unimplemented"

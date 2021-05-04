type stock = {
  ticker : string;
  shares : int;
  net_change : float;
  day_change : float;
}

type t = {
  liquidity : float;
  stocks : (string, stock) Hashtbl.t;
  net_worth : float;
  net_change : float;
  day_change : float;
}

let portfolio_gain_loss (portfolio : t) : float = portfolio.net_change

let portfolio_gain_loss_day (portfolio : t) : float = portfolio.day_change

let net_worth (portfolio : t) : float = portfolio.net_worth

let liquidity (portfolio : t) : float = portfolio.liquidity

let shares (stock : stock) : int = stock.shares

let ticker (stock : stock) : string = stock.ticker

let stock_gain_loss (stock : stock) : float = stock.net_change

let stock_gain_loss_day (stock : stock) : float = stock.day_change

let list_of_tickers (portfolio : t) : string list = failwith "unimplemented"

let list_of_stocks (portfolio : t) : stock list = failwith "unimplemented"

let stock_from_ticker (portfolio : t) (ticker : string) : stock option =
  if Hashtbl.mem portfolio.stocks ticker then None
  else Some (Hashtbl.find portfolio.stocks ticker)

let change_ticker_shares (portfolio : t) (ticker : string) (shares : int)
    (liquidity : float) : t =
    let stock = stock_from_ticker portfolio ticker in
    match stock with
    | None -> portfolio
    | Some s -> begin
      let new_stocks = Hashtbl.remove portfolio.stocks ticker in 
      let stock = {ticker: ticker s; shares: (shares s + shares); net_change: stock_gain_loss s; day_change: stock_gain_loss_day s} in 
      {t.liquidity}

    end

let change_stock_shares (portfolio : t) (stock : stock) (shares : int)
    (liquidity : float) : t =
  failwith "unimplemented"

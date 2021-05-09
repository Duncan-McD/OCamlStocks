type stock = unit

type t = unit

let portfolio_gain_loss (portfolio : t) : float = failwith "unimplemented"

let portfolio_gain_loss_day (portfolio : t) : float = failwith "unimplemented"

let net_worth (portfolio : t) : float = failwith "unimplemented"

let liquidity (portfolio : t) : float = failwith "unimplemented"

let shares (stock : stock) : int = failwith "unimplemented"

let ticker (stock : stock) : string = failwith "unimplemented"

let stock_gain_loss (stock : stock) : float = failwith "unimplemented"

let stock_gain_loss_day (stock : stock) : float = failwith "unimplemented"

let list_of_tickers (portfolio : t) : string list = failwith "unimplemented"

let list_of_stocks (portfolio : t) : stock list = failwith "unimplemented"

let stock_from_ticker (portfolio : t) (ticker : string) : stock =
  failwith "unimplemented"

let change_ticker_shares (portfolio : t) (ticker : string) (shares : int)
    (liquidity : float) : t =
  failwith "unimplemented"

let change_stock_shares (portfolio : t) (stock : stock) (shares : int)
    (liquidity : float) : t =
  failwith "unimplemented"

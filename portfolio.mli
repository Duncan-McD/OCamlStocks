(** {2 Types}*)

type t
(** The abstract data type for a stock portfolio *)

type stock
(** The abstract data type for a stock in a portfolio*)

(** {2 Portfolio Functions}*)

val portfolio_gain_loss : t -> float
(** [portfolio_gain_loss p] is the net gain or loss in the portfolio [p] since creation*)

val portfolio_gain_loss_day : t -> float
(** [portfolio_gain_loss_day p] is the net gain or loss of portfolio [p] in the most recent day *)

val net_worth : t -> float
(** [net_worth p] is the net worth of stock portfolio [p]*)

val liquidity : t -> float
(** [liquidity p] is the amount of free liquidity the portfolio [p] has*)

(** {2 Stock Functions}*)

val shares : stock -> int
(** [shares s] is the amount of shares of a given stock *)

val ticker : stock -> string
(** [ticker s] is the stock ticker of stock [s]*)

val stock_gain_loss : stock -> float
(** [stock_gain_loss s] is the net gain or loss of a stock [s] since being bought*)

val stock_gain_loss_day : stock -> float
(** [stock_gain_loss_day s] is the net gain or loss of a stock [s] in the most recent day*)

(** {2 Getter Functions}*)

val list_of_tickers : t -> string list
(** [list_of_tickers p] is a list of stock tickers in portfolio [p]*)

val list_of_stocks : t -> stock list
(** [list_of_stocks p] is a list of stocks in portfolio [p]*)

val stock_from_ticker : t -> string -> stock option
(** [stock_from_ticker p t] is the Some stock in portfolio [p] with stock ticker 
    [t] or None if this ticker is not in [p]*)

(** {2 Actions}*)

val change_ticker_shares : t -> string -> int -> float -> t
(** [add_shares p t sh l] adds [sh] shares to a stock with ticker [t] and adds the 
    resulting change in liquidity [l] to the liquidity of portfolio [p] 
    
    Note: [sh] and [l] can be positive or negative*)

val change_stock_shares : t -> stock -> int -> float -> t
(** [add_shares p s sh l] adds [sh] shares to a stock [s] and adds the 
    resulting change in liquidity [l] to the liquidity of portfolio [p] 
    
    Note: [sh] and [l] can be positive or negative*)

(** {2 Types}*)

type t
(** The abstract data type for a stock portfolio *)

type stock
(** The abstract data type for a stock in a portfolio*)

(** {2 Portfolio Functions}*)

val empty_portfolio : t
(** [empty_portfolio] is an initial empty portfolio*)

val portfolio_gain_loss : t -> float
(** [portfolio_gain_loss p] is the net gain or loss in the portfolio [p] since last refresh*)

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
(** [stock_gain_loss s] is the net gain or loss of a stock [s] since last refresh*)

(** {2 Getter Functions}*)

val list_of_tickers : t -> string list
(** [list_of_tickers p] is a list of stock tickers in portfolio [p]*)

val list_of_stocks : t -> stock list
(** [list_of_stocks p] is a list of stocks in portfolio [p]*)

val stock_from_ticker : t -> string -> stock option
(** [stock_from_ticker p t] is the Some stock in portfolio [p] with stock ticker 
    [t] or None if this ticker is not in [p]*)

(** {2 Actions}*)

val refresh : t -> t
(** [refresh p] is the portfolio p but with refreshed data based on newer stock data*)

val process : t -> (string * float) list * string list -> t
(** [process p l] is the portfolio p but with the stocks in l processed 
    - l is the output of Algorithm.ml*)

val copy : t -> t
(** [copy p] is a copy of portfolio p with different references for the values 
    (this is needed because the stocks field of a portfolio is mutable)*)

val compare : t -> t -> float
(** [compare p1 p2] will return a positive float if p1 has a larger net worth than p2, 
    a negative float if p1 has a smaller net worth than p2, 
    and 0 if they have the same net worth*)

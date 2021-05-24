(** Representation of a portfolio  *)

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

val change_liquidity : t -> float -> t
(** [change_liquidity p f] is portfolio p with f added to your current liquidity*)

(** {2 Stock Functions}*)

val shares : stock -> float
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

val compare : t -> t -> int
(** [compare p1 p2] will return 1 if p1 has a larger net worth than p2, 
    a -1 if p1 has a smaller net worth than p2, 
    and 0 if they have the same net worth*)

val vars : t -> float * float * float * float
(**[vars p] is the tuple of constants used by algorithm to generate this portfolio*)

val portfolio_of_json : Yojson.Basic.t -> t

(**[portfolio_of_json j] is the portfolio representation of json [j]*)

val to_json_string : t -> string

(**[to_json_string t] is the string in json format of portfolio [t]*)
val sell_all : t -> t
(** [sell_all p] sells all the stocks in portfolio p*)

val value : stock -> float
(** [value s] is the current evaluation of your holding in stock s*)

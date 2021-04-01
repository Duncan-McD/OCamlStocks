(** Represents the most current data of any stock pulled from Yahoo Finance *)

type t
(** the abstract type representation of stock data *)

exception StockNotFound of string
(** Raised when a stock is not found on Yahoo Finance *)

val stockdata_from_ticker : string -> t
(** [stockdata_from_ticker t] is the current stock data of stock ticker [t] 
    Raises: [StockNotFound s] if the stock represented by ticker [t] is not
      on Yahoo Finance*)

val value : t -> float
(** [value sd] is the current value of the stock represented by [sd] *)

val change : t -> float
(** [change sd] is the percent change in value of the stock represented by [sd] *)

val ticker : t -> string
(** [ticker sd] is the ticker of the stock represented by [sd] *)

(** Represents the most current data of any stock pulled from Yahoo Finance *)

(** the abstract type representation of stock data *)
type t

(** Raised when a stock is not found on Yahoo Finance *)
exception StockNotFound of string

(** [stockdata_from_ticker t] is the current stock data of stock ticker [t] 
    Raises: [StockNotFound s] if the stock represented by ticker [t] is not
      on Yahoo Finance*)
val stockdata_from_ticker : string -> t

(** [value sd] is the current value of the stock represented by [sd] *)
val value : t -> float

(** [change sd] is the percent change in value of the stock represented by [sd] *)
val change : t -> float
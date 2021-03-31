(** Represents the most current data of any stock *)

(** the abstract type representation of stock data *)
type t

(** [stockdata_from_ticker t] is the current stock data of stock ticker [t] *)
val stockdata_from_ticker : string -> t

(** [stockdata_from_stock s] is the current stock data of stock [s]*)
val stockdata_from_stock : string -> Parser.stock

(** [value sd] is the current value of the stock data [sd] *)
val value : t -> float
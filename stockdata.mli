(** Represents the most current data of any stock *)

(** the abstract type representation of stock data *)
type t

(** [stockdata_from_ticker t] is the current stock data of stock ticker [t] *)
val stockdata_from_ticker : string -> t

(** [value sd] is the current value of the stock represented by [sd] *)
val value : t -> float

(** [change sd] is the percent change in value of the stock represented by [sd] *)
val change : t -> float
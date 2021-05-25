(** Represents the most current data of any stock pulled from Yahoo Finance *)

(** {1 Type} *)

type t
(** the abstract type representation of stock data *)

(** {1 Exception} *)

exception StockNotFound of string
(** Raised when a stock is not found on Yahoo Finance *)

(** {1 Constructor } *)

val stockdata_from_ticker : string -> t option
(** [stockdata_from_ticker t] is Some the current stock data of stock ticker [t] 
    otherwise None if not found on Yahoo Finance *)

(** {5 Getter Functions }*)

val value : t -> float
(** [value sd] is the current value of the stock represented by [sd] *)

val change : t -> float
(** [change sd] is the percent change in value of the stock represented by 
    [sd] *)

val ticker : t -> string
(** [ticker sd] is the ticker of the stock represented by [sd] *)

val rating : t -> float
(** [rating sd] is the rating of the stock represented by [sd] from Yahoo 
    Finance *)

val require : string -> t option -> t
(** [require s t] returns t' if t is Some t' and raised StockNotFound error if 
    t is None for ticker s*)

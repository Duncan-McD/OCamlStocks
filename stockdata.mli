(** Represents the most current data of any stock pulled from Yahoo Finance *)

(** {2 Types} *)

type t
(** the abstract type representation of stock data *)

(** {2 Exceptions} *)

exception StockNotFound of string
(** Raised when a stock is not found on Yahoo Finance *)

(** {2 Constructor } *)

val stockdata_from_ticker : string -> t option
(** [stockdata_from_ticker t] is Some the current stock data of stock ticker [t] 
    otherwise None if not found on Yahoo Finance *)

(** {2 Getter Functions }*)

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

val require : t option -> t
(** [require t] returns t' if t is Some t' and raised StockNotFound error if 
    t is None *)

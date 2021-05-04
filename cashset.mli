(** Represents a hashset of all stocks on the stock market that are NOT in commonwords.csv *)

(** {2 Table}*)

val cashtable : (string, int) Hashtbl.t

(** {2 Functions} *)

val is_stock_name : string -> bool
(** [is_stock_name s] is true if [s] is the name of a stock on the stock market *)

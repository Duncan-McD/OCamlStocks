(** {2 Types}*)

type t

type stock

(** {2 Portfolio Functions}*)

val portfolio_gain_loss : t -> float

val net_worth : t -> float

(** {2 Stock Functions}*)

val shares : stock -> int

val stock_gain_loss : stock -> float

(** {2 Getter Functions}*)

val list_of_tickers : t -> string list

val list_of_stocks : t -> stock list

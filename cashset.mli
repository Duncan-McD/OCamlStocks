(*cashset *)

type cashtable = (int, int) Hashtbl.t

(*check if stock name is on the stock market*)
val is_stock_name : string -> bool

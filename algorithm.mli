(** Processes parsed stock data list into a weighted stock list *)

val get_stocks_to_sell_list :
  float -> float -> float -> float -> Parser.stocks -> (string * float) list

val get_stocks_to_buy_list :
  float -> float -> float -> float -> Parser.stocks -> (string * float) list

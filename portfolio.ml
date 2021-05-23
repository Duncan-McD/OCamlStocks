type stock = {
  ticker : string;
  shares : float;
  price_per_share : float;
  initial_value : float;
  value : float;
  change : float;
}

type t = {
  liquidity : float;
  stocks : (string, stock) Hashtbl.t;
  net_worth : float;
  change : float;
  first : bool;
  vars : float * float * float * float;
}

let portfolio_gain_loss (portfolio : t) : float = failwith "unimplemented"

let portfolio_gain_loss_day (portfolio : t) : float = failwith "unimplemented"

let net_worth (portfolio : t) : float = failwith "unimplemented"

let liquidity (portfolio : t) : float = failwith "unimplemented"

let shares (stock : stock) : int = failwith "unimplemented"

let ticker (stock : stock) : string = failwith "unimplemented"

let stock_gain_loss (stock : stock) : float = failwith "unimplemented"

let stock_gain_loss_day (stock : stock) : float = failwith "unimplemented"

let list_of_tickers (portfolio : t) : string list = failwith "unimplemented"

let list_of_stocks (portfolio : t) : stock list = failwith "unimplemented"

let stock_from_ticker (portfolio : t) (ticker : string) : stock =
  failwith "unimplemented"

let change_ticker_shares (portfolio : t) (ticker : string) (shares : int)
    (liquidity : float) : t =
  failwith "unimplemented"

let change_stock_shares (portfolio : t) (stock : stock) (shares : int)
    (liquidity : float) : t =
  failwith "unimplemented"

let liquidity_stocklist_break = ",,"

let stocklist_networth_break = ",,"

let networth_change_break = ",,"

let change_first_break = ",,"

let ticker_shares_break = ",,"

let shares_pricepershare_break = ",,"

let pricepershare_initialvalue_break = ",,"

let initialvalue_value_break = ",,"

let value_change_break = ",,"

let ocomma_of_stock stock =
  stock.ticker ^ ticker_shares_break
  ^ string_of_float stock.shares
  ^ shares_pricepershare_break
  ^ string_of_float stock.price_per_share
  ^ pricepershare_initialvalue_break
  ^ string_of_float stock.initial_value
  ^ initialvalue_value_break

let ocomma_of_tuple tup = failwith "unimplimented"

let rec ocomma_of_stocklist stocklist acc =
  match stocklist with
  | [] -> acc
  | h :: t -> ocomma_of_stocklist t (acc ^ ocomma_of_stock h)

let ocomma_of_portfolio t =
  "," ^ "Portfolio" ^ ":" ^ ","
  ^ string_of_float t.liquidity
  ^ ","
  ^ ocomma_of_stocklist (list_of_stocks t) ""
  ^ ","
  ^ string_of_float t.net_worth
  ^ "," ^ string_of_float t.change ^ "," ^ string_of_bool t.first ^ ","
  ^ ocomma_of_tuple t.vars

let file = "portfolio_history.txt"

let save_current_portfolio portfolio =
  let str = ocomma_of_portfolio portfolio in
  let oc = open_out file in
  Printf.fprintf oc "%s\n" str;
  close_out oc

(* Read file and display the first line *)
let load_last_portfolio =
  let read_message_of_file = open_in file in
  try
    let line = input_line read_message_of_file in
    (* read line, discard \n *)
    print_endline line;
    (* write the result to stdout *)
    flush stdout;
    (* write on the underlying device now *)
    close_in read_message_of_file
    (* close the input channel *)
  with e ->
    (* some unexpected exception occurs *)
    close_in_noerr read_message_of_file;
    (* emergency closing *)
    raise e

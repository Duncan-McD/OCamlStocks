type t = {
  value: float;
  change: float;
}

let stockdata_from_ticker (ticker: string) : t = failwith "unimplemented"

let stockdata_from_stock (stock: Parser.stock) : t = failwith "unimplemented"

let value (stock_data : t) : float = failwith "unimplemented"

let change (stock_data : t) : float = failwith "unimplemented"
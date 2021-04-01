(** [cashtable] is the hashtable that stores the hashed names of stocks as keys with value 0 *)
let cashtable = Hashtbl.create 10624

(** [word_hashtable] is the hashtable that stores the hashed names of common words that may be the same as a stock ticker 0 *)
let word_hashtbl = Hashtbl.create 50

let stock_file = "stocks.csv"

let words_file = "commonwords.csv"

(** [parse_line str] is the string that occurs before the ',' in a given string *)
let rec parse_line str =
  let endat = String.index str ',' in
  String.sub str 0 endat

(** [build_word_hashtable csv_line] adds [csv_line] to [word_hashtable]*)
let build_word_hashtable (csv_line : string) =
  Hashtbl.add word_hashtbl (Hashtbl.hash csv_line) 0

(** [add_to_stock_hashtbl csv_line] adds [csv_line] to [cashtable] and adds ($ ^ [csv_line]) to [cashtable] if [csv_line] is not in [word_hashtl] *)
let add_to_stock_hashtbl (csv_line : string) =
  if Hashtbl.mem word_hashtbl (Hashtbl.hash csv_line) = false then
    Hashtbl.add cashtable (Hashtbl.hash csv_line) 0;
  Hashtbl.add cashtable (Hashtbl.hash ("$" ^ csv_line)) 0

(** constructs [word_hashtbl] from commonwords.csv; constructs [cashtable]*)
let () =
  let ic = open_in words_file in
  for i = 1 to 49 do
    try
      let line = input_line ic in
      line |> parse_line |> build_word_hashtable
    with e ->
      close_in_noerr ic;
      raise e
  done;
  close_in ic;
  let ic = open_in stock_file in
  for i = 1 to 10624 do
    try
      let line = input_line ic in
      line |> parse_line |> add_to_stock_hashtbl
    with e ->
      close_in_noerr ic;
      raise e
  done;
  close_in ic

(** [is_stock_name s] is true if [s] is the name of a stock on the stock market *)
let is_stock_name (name : string) = Hashtbl.mem cashtable (Hashtbl.hash name)

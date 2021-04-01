(* convert csv file to a a hashmap inside of a .ml file *)
(* get each line of the Csvfile until there is no longer a line*)
(* call add_to_hastbl on each line*)
let cashtable = Hashtbl.create 7337

let word_hashtbl = Hashtbl.create 50

let stock_file = "stocks.csv"

let words_file = "commonwords.csv"

let rec parse_line str =
  let endat = String.index str ',' in
  String.sub str 0 endat

let build_word_hashtable (csv_line : string) =
  Hashtbl.add word_hashtbl (Hashtbl.hash csv_line) 0

let add_to_stock_hashtbl (csv_line : string) =
  if Hashtbl.mem word_hashtbl (Hashtbl.hash csv_line) = false then
    Hashtbl.add cashtable (Hashtbl.hash csv_line) 0;
  Hashtbl.add cashtable (Hashtbl.hash ("$" ^ csv_line)) 0

let () =
  (* Read file and display the first line *)
  let ic = open_in words_file in
  for i = 1 to 49 do
    try
      (* read line, discard \n *)
      let line = input_line ic in
      (*add parsed line to hashtbl*)
      line |> parse_line |> build_word_hashtable
    with e ->
      (* some unexpected exception occurs *)
      close_in_noerr ic;
      (* emergency closing *)
      raise e
  done;
  close_in ic;
  let ic = open_in stock_file in
  for i = 1 to 7337 do
    try
      (* read line, discard \n *)
      let line = input_line ic in
      (*add parsed line to hashtbl*)
      line |> parse_line |> add_to_stock_hashtbl
    with e ->
      (* some unexpected exception occurs *)
      close_in_noerr ic;
      (* emergency closing *)
      raise e
  done;
  close_in ic

let is_stock_name (name : string) = Hashtbl.mem cashtable (Hashtbl.hash name)

(* exit with error: files are closed but
   channels are not flushed *)

(* normal exit: all channels are flushed and closed *)

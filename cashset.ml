(* convert csv file to a a hashmap inside of a .ml file *)
(* get each line of the Csvfile until there is no longer a line*)
(* call add_to_hastbl on each line*)
let stockname_hashtbl = Hashtbl.create 3300

let file = "stocks.csv"

let rec parse_line str =
  let endat = String.index str ',' in
  String.sub str 0 endat

let add_to_hastbl (csv_line : string) =
  Hashtbl.add stockname_hashtbl (Hashtbl.hash csv_line) 0

let () =
  (* Read file and display the first line *)
  let ic = open_in file in
  for i = 1 to 3299 do
    try
      (* read line, discard \n *)
      let line = input_line ic in
      (*add parsed line to hashtbl*)
      line |> parse_line |> add_to_hastbl
    with e ->
      (* some unexpected exception occurs *)
      close_in_noerr ic;
      (* emergency closing *)
      raise e
  done;
  close_in ic

let is_stock_name (name : string) =
  Hashtbl.mem stockname_hashtbl (Hashtbl.hash name)

(* exit with error: files are closed but
   channels are not flushed *)

(* normal exit: all channels are flushed and closed *)

let file = "testreadwrite.txt"

let write_message_to_file str =
  (* Write message to file *)
  let oc = open_out file in
  (* create or truncate file, return channel *)
  Printf.fprintf oc "%s\n" str;
  (* write something *)
  close_out oc;

  (* flush and close the channel *)

  (* Read file and display the first line *)
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

(* exit with error: files are closed but
   channels are not flushed *)

(* normal exit: all channels are flushed and closed *)

let make_stock_history_csv parsed_subreddit =
  let oc = open_out "lastHistoryScore.txt" in
  let stockname_list = Parser.stock_names parsed_subreddit in
  let rec write_parsed_subreddit list_of_stocks p_subreddit =
    match list_of_stocks with
    | [] -> ()
    | h :: t ->
        Printf.fprintf oc "%s\n"
          (h ^ ", "
          ^ string_of_float (fst (Parser.data parsed_subreddit h))
          ^ "0");
        write_parsed_subreddit t p_subreddit
  in
  write_parsed_subreddit stockname_list parsed_subreddit;
  close_out oc

let make_stock_hist_csv_test str =
  let scraped2 =
    Scraper.scrape_json ~amount:25 "testing_files/stocksnew.json"
  in
  let parsed = Parser.parse scraped2 in
  make_stock_history_csv parsed;
  print_endline str

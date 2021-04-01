open Stockdata

let rec pretty_print_stock_data stock_data =
  print_endline "";
  print_endline ("STOCK TICKER: " ^ ticker stock_data);
  print_endline ("STOCK VALUE: " ^ string_of_float (value stock_data));
  print_endline ("STOCK CHANGE: " ^ string_of_float (change stock_data));
  print_endline ""

let rec ask_for_pretty_print stock_data =
  print_endline "Do you want to pretty print this stock data? [Y/N]";
  let response = read_line () in
  if response = "Y" then pretty_print_stock_data stock_data
  else if response = "N" then ()
  else (
    print_endline "Invalid Input";
    ask_for_pretty_print stock_data)

let rec prompt_for_stock () =
  let stock_data = prompt_and_get_stock_data () in
  ask_for_pretty_print stock_data;
  ask_if_want_new ()

and ask_if_want_new () =
  print_endline "Do you want to get another stock's data? [Y/N]";
  let response = read_line () in
  if response = "Y" then (
    try prompt_for_stock ()
    with StockNotFound s ->
      print_endline
        ("Could not find stock " ^ s ^ " on Yahoo Finance. Try again.");
      main ())
  else if response = "N" then ()
  else (
    print_endline "Invalid Input";
    ask_if_want_new ())

and prompt_and_get_stock_data () =
  print_endline
    "Enter a stock ticker to retrieve its data from Yahoo Finance - Example: \
     GME not \"GME\"";
  let ticker = read_line () in
  stockdata_from_ticker ticker

and main () =
  try prompt_for_stock ()
  with StockNotFound s ->
    print_endline ("Could not find stock " ^ s ^ " on Yahoo Finance. Try again.");
    main ()

let () = main ()

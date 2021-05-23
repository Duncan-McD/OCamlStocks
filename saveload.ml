open Yojson
let j = Yojson.Basic.from_file "data.json"
(*take in an object*)

let save_user (user:string) = 
  

let save_testing_portfolio  =
  (* Write message to file *)
  let oc = open_out file in
  (* create or truncate file, return channel *)
  Printf.printf oc "%s\n" str;
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
let save_to_file = 

  let file = "portfolio_history.txt"

  (*let save_current_portfolio portfolio =
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
*)
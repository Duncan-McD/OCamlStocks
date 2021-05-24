type t = {
  number_of_tests : int;
  subreddit_list : string list;
  posts_per_scrape : int;
}

let default =
  {
    number_of_tests = 1000;
    subreddit_list = [ "r/stocks"; "r/investing" ];
    posts_per_scrape = 100;
  }

let number_of_tests (config : t) : int = config.number_of_tests

let subreddit_list (config : t) : string list = config.subreddit_list

let posts_per_scrape (config : t) : int = config.posts_per_scrape

open Yojson.Basic.Util

let rec string_of_subreddit_list (subreddit_list : string list) acc =
  match subreddit_list with
  | [] -> acc
  | h :: t -> string_of_subreddit_list t (h ^ ", " ^ acc)

let to_json_string t =
  "{" ^ "\"number_of_tests: \""
  ^ string_of_int t.number_of_tests
  ^ ", " ^ "\"subreddit_list: \"" ^ "["
  ^ string_of_subreddit_list t.subreddit_list ""
  ^ ", " ^ "\"posts_per_scrape: \""
  ^ string_of_int t.posts_per_scrape
  ^ ", "

let rec subreddit_list_of_json (j : Yojson.Basic.t list) acc =
  match j with
  | [] -> acc
  | h :: t -> subreddit_list_of_json t (to_string h :: acc)

let config_of_json (j : Yojson.Basic.t) =
  {
    number_of_tests = int_of_string (to_string (member "number_of_tests" j));
    subreddit_list =
      subreddit_list_of_json (to_list (member "subreddit_list" j)) [];
    posts_per_scrape = int_of_string (to_string (member "posts_per_scrape" j));
  }

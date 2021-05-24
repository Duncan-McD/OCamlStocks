open Yojson.Basic
open Yojson.Basic.Util

type subreddit = int * Scraper.subreddit_ordering * string

type t = {
  subreddits : subreddit list;
  optimizing : bool;
  consts : float * float * float * float;
  num_test : int;
  liquidity : float;
}

let default () =
  {
    subreddits = [ (50, Hot, "r/stocks"); (50, Hot, "r/investing") ];
    optimizing = true;
    consts = (1., 1., 1., 1.);
    num_test = 10000;
    liquidity = 0.;
  }

let add_subreddit config subreddit =
  {
    subreddits = subreddit :: config.subreddits;
    optimizing = config.optimizing;
    consts = config.consts;
    num_test = config.num_test;
    liquidity = config.liquidity;
  }

let set_subreddits config new_subreddits =
  {
    subreddits = new_subreddits;
    optimizing = config.optimizing;
    consts = config.consts;
    num_test = config.num_test;
    liquidity = config.liquidity;
  }

let set_optimizing config b =
  {
    subreddits = config.subreddits;
    optimizing = b;
    consts = config.consts;
    num_test = config.num_test;
    liquidity = config.liquidity;
  }

let set_consts config (a, b, c, d) =
  {
    subreddits = config.subreddits;
    optimizing = config.optimizing;
    consts = (a, b, c, d);
    num_test = config.num_test;
    liquidity = config.liquidity;
  }

let set_tests config tests =
  {
    subreddits = config.subreddits;
    optimizing = config.optimizing;
    consts = config.consts;
    num_test = tests;
    liquidity = config.liquidity;
  }

let set_liquidity config new_liquidity =
  {
    subreddits = config.subreddits;
    optimizing = config.optimizing;
    consts = config.consts;
    num_test = config.num_test;
    liquidity = new_liquidity;
  }

let subreddit_info config = config.subreddits

let is_optimizing config = config.optimizing

let consts config = config.consts

let num_tests config = config.num_test

let liquidity config = config.liquidity

let fst3 (t : int * Scraper.subreddit_ordering * string) =
  match t with a, b, c -> a

let snd3 (t : int * Scraper.subreddit_ordering * string) =
  match t with a, b, c -> b

let trd3 (t : int * Scraper.subreddit_ordering * string) =
  match t with a, b, c -> c

let string_of_subreddit_ordering (ordering : Scraper.subreddit_ordering) =
  match ordering with
  | Hot -> "Hot"
  | New -> "New"
  | Rising -> "Rising"
  | Top (s : Scraper.time) -> (
      match s with
      | Now -> "Now"
      | Today -> "Today"
      | ThisWeek -> "ThisWeek"
      | ThisMonth -> "ThisMonth"
      | ThisYear -> "ThisYear"
      | AllTime -> "AllTime")

(**[string_of_stocklist s] is the string in json format of stocklist [s]*)
let string_of_subreddit (sub : subreddit) =
  "{" ^ "\"num_posts: \""
  ^ string_of_int (fst3 sub)
  ^ "," ^ "\"ordering: \""
  ^ string_of_subreddit_ordering (snd3 sub)
  ^ "," ^ "\"url: \"" ^ trd3 sub ^ "}"

let rec string_of_subreddits subreddit_list acc =
  match subreddit_list with
  | [] -> acc
  | h :: t -> string_of_subreddits t (string_of_subreddit h ^ "," ^ acc)

(**[fst4 t] is the first element in 4-tuple [t]*)
let fst4 (quad : float * float * float * float) =
  match quad with a, b, c, d -> a

(**[snd4 t] is the second element in 4-tuple [t]*)
let snd4 (quad : float * float * float * float) =
  match quad with a, b, c, d -> b

(**[trd4 t] is the third element in 4-tuple [t]*)
let trd4 (quad : float * float * float * float) =
  match quad with a, b, c, d -> c

(**[fth4 t] is the fourth element in 4-tuple [t]*)
let fth4 (quad : float * float * float * float) =
  match quad with a, b, c, d -> d

(**[string_of_vars t] is the string in json format of vars [t]*)
let string_of_const (config : t) =
  "[" ^ "\"x: \""
  ^ string_of_float (fst4 config.consts)
  ^ "," ^ "\"y: \""
  ^ string_of_float (snd4 config.consts)
  ^ "," ^ "\"w1: \""
  ^ string_of_float (trd4 config.consts)
  ^ "," ^ "\"w2: \""
  ^ string_of_float (trd4 config.consts)
  ^ "]"

let to_json_string (config : t) =
  "{" ^ "\"subreddits: \"" ^ "["
  ^ string_of_subreddits config.subreddits ""
  ^ "]" ^ "," ^ "\"optimizing: \""
  ^ string_of_bool config.optimizing
  ^ "," ^ "\"consts: \"" ^ string_of_const config ^ "," ^ "\"num_test: \""
  ^ string_of_int config.num_test
  ^ "," ^ "\"liquidity: \""
  ^ string_of_float config.liquidity
  ^ "}"

let ordering_of_json (str : string) : Scraper.subreddit_ordering =
  match str with
  | s when s = "Hot" -> Hot
  | s when s = "New" -> New
  | s when s = "Rising" -> Rising
  | s when s = "Now" -> Top Now
  | s when s = "Today" -> Top Today
  | s when s = "ThisWeek" -> Top ThisWeek
  | s when s = "ThisMonth" -> Top ThisMonth
  | s when s = "ThisYear" -> Top ThisYear
  | s when s = "AllTime" -> Top AllTime
  | _ -> failwith "impossible"

let subreddit_of_json (j : Yojson.Basic.t) : subreddit =
  ( int_of_string (to_string (member "num_posts" j)),
    ordering_of_json (to_string (member "ordering" j)),
    to_string (member "url" j) )

let rec subreddits_of_json (j : Yojson.Basic.t list) acc =
  match j with
  | [] -> acc
  | h :: t -> subreddits_of_json t (subreddit_of_json h :: acc)

let consts_of_json (j : Yojson.Basic.t) =
  ( float_of_string (to_string (member "x" j)),
    float_of_string (to_string (member "y" j)),
    float_of_string (to_string (member "w1" j)),
    float_of_string (to_string (member "w2" j)) )

let config_of_json (j : Yojson.Basic.t) =
  {
    subreddits = subreddits_of_json (to_list (member "subreddits" j)) [];
    optimizing = bool_of_string (to_string (member "optimizing" j));
    consts = consts_of_json (member "consts" j);
    num_test = int_of_string (to_string (member "num_test" j));
    liquidity = float_of_string (to_string (member "liquidity" j));
  }

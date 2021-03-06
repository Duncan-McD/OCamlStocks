open Yojson.Basic
open Yojson.Basic.Util

type subreddit = int * Scraper.subreddit_ordering * string

type t = {
  subreddits : subreddit list;
  optimizing : bool;
  consts : float * float * float * float;
  num_test : int;
}

let default =
  {
    subreddits = [ (50, Hot, "r/stocks"); (50, Hot, "r/investing") ];
    optimizing = true;
    consts = (1., 1., 1., 1.);
    num_test = 10000;
  }

let add_subreddit config subreddit =
  {
    subreddits = subreddit :: config.subreddits;
    optimizing = config.optimizing;
    consts = config.consts;
    num_test = config.num_test;
  }

let set_subreddits config new_subreddits =
  {
    subreddits = new_subreddits;
    optimizing = config.optimizing;
    consts = config.consts;
    num_test = config.num_test;
  }

let set_optimizing config b =
  {
    subreddits = config.subreddits;
    optimizing = b;
    consts = config.consts;
    num_test = config.num_test;
  }

let set_consts config (a, b, c, d) =
  {
    subreddits = config.subreddits;
    optimizing = config.optimizing;
    consts = (a, b, c, d);
    num_test = config.num_test;
  }

let set_tests config tests =
  {
    subreddits = config.subreddits;
    optimizing = config.optimizing;
    consts = config.consts;
    num_test = tests;
  }

let subreddit_info config = config.subreddits

let is_optimizing config = config.optimizing

let consts config = config.consts

let num_tests config = config.num_test

(** [fst3 (a, b, c)] is a *)
let fst3 (t : int * Scraper.subreddit_ordering * string) =
  match t with a, b, c -> a

(** [snd3 (a, b, c)] is b *)
let snd3 (t : int * Scraper.subreddit_ordering * string) =
  match t with a, b, c -> b

(** [trd3 (a, b, c)] is c *)
let trd3 (t : int * Scraper.subreddit_ordering * string) =
  match t with a, b, c -> c

(** [string_of_subreddit_ordering ordering] is a string version of [ordering] *)
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
let consts_to_json (config : t) =
  `Assoc
    [
      ("x", `Float (fst4 config.consts));
      ("y", `Float (snd4 config.consts));
      ("w1", `Float (trd4 config.consts));
      ("w2", `Float (fth4 config.consts));
    ]

let subreddit_to_json (subreddit : subreddit) =
  `Assoc
    [
      ("num_posts", `Int (fst3 subreddit));
      ("ordering", `String (string_of_subreddit_ordering (snd3 subreddit)));
      ("url", `String (trd3 subreddit));
    ]

(** [subreddits_to_json subreddits] is a list with each subreddit in 
    [subreddits] converted to json *)
let rec subreddits_to_json (subreddit_list : subreddit list) acc =
  match subreddit_list with
  | [] -> `List acc
  | h :: t -> subreddits_to_json t (subreddit_to_json h :: acc)

let to_json (config : t) =
  `Assoc
    [
      ("subreddits", subreddits_to_json config.subreddits []);
      ("optimizing", `Bool config.optimizing);
      ("consts", consts_to_json config);
      ("num_test", `Int config.num_test);
    ]

(** [ordering_of_json json_str] is a [Scraper.subreddit_ordering] 
    from [json_str] *)
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

(** [subreddit_of_json json] is a subreddit from the [json] *)
let subreddit_of_json (j : Yojson.Basic.t) : subreddit =
  ( to_int (member "num_posts" j),
    ordering_of_json (to_string (member "ordering" j)),
    to_string (member "url" j) )

(** [subreddits_of_json json] is a subreddit list from [json] *)
let rec subreddits_of_json (j : Yojson.Basic.t list) acc =
  match j with
  | [] -> List.rev acc
  | h :: t -> subreddits_of_json t (subreddit_of_json h :: acc)

(** [consts_of_json json] is the consts quad from [json] *)
let consts_of_json (j : Yojson.Basic.t) =
  ( to_float (member "x" j),
    to_float (member "y" j),
    to_float (member "w1" j),
    to_float (member "w2" j) )

let config_of_json (j : Yojson.Basic.t) =
  {
    subreddits = subreddits_of_json (to_list (member "subreddits" j)) [];
    optimizing = to_bool (member "optimizing" j);
    consts = consts_of_json (member "consts" j);
    num_test = to_int (member "num_test" j);
  }

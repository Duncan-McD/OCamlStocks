(** Representation of a bot's configuration *)

(** {1 Type}*)

type t
(** The abstract data type for a program configuration *)

val default : t
(**[default] is the default configuration of config, with [number_of_tests] being 1000,
    [subreddit_list] consisting of "r/stocks" and "r/investing", and
    [posts_per_scrape] being 100*)

(** {1 Config Function}*)

val to_json_string : t -> string
(** [to_json_string t] is the representation of [t] as a string in the json formatt*)

(* Add update config function *)

(** {3 Getter Functions}*)

val number_of_tests : t -> int
(** [number_of_tests t] is the number of portfolios that will be created when [initialize_testing_portfolios] is run*)

val subreddit_list : t -> string list
(** [subreddit_list t] is the list of subreddits that will be scraped *)

val posts_per_scrape : t -> int
(** [posts_per_scrape t] is the number of posts that will be scraped in each subreddit *)

(** {1 Setter Function}*)

val config_of_json : Yojson.Basic.t -> t
(** [config_of_json j] is the config representation of json [j]*)

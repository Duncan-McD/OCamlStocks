(** Representation of a Subreddit Parser *)

(** {2 Types} *)

type post
(** The abstract type of relevant data about a post *)

type stocks
(** The abstract type of the list of stocks and their relevant data *)

(** {2 Constructor} *)

val parse : Scraper.subreddit -> stocks
(** [parse sub] is a list of stocks. It parses the scraped subreddit [sub] to 
    find mentioned stocks and  gets relevant data from each post it is 
    mentioned in *)

(** {2 Functions on stocks type} *)

val stock_names : stocks -> string list
(** [stock_names stocks] is the list of stock ticker symbols of all stocks in [stocks] *)

val data : stocks -> string -> float * post list
(** [data stocks_data stock_name] is the relevant data associated with [stock_name] in [stock_data]*)

val join_stocks : stocks list -> stocks
(** [join_stocks stocks_list] is a type [stocks] created by joining the data 
    from each [stocks] in [stocks_list]  *)

(** {2 Functions on post type} *)

val upvote_score : post -> int
(** [score post] is the upvotes minus downvotes of [post] *)

val upvote_ratio : post -> float
(** [upvote_ratio post] is the ratio of upvotes to total votes of [post] *)

val connotation : post -> float
(** [connotation post] is the connotation of language of [post] 
    The range of possible connotations is -1 to 1 from worst to best *)

val history_score : Stockdata.t option -> float
(** [history-score stockdata]*)

(** {2 Alternate function forms} *)

val connotation_str : string -> float
(** [connotation str] is the connotation of string [str] 
    The range of possible connotations is -1 to 1 from worst to best *)

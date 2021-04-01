(** Representation of a Subreddit Parser *)

(** The abstract type of relevant data about a post *)
type post

(** The abstract type of the list of stocks and their relevant data *)
type stocks

(** [parse sub] is a list of stocks. It parses the scraped subreddit [sub] to 
    find mentioned stocks and  gets relevant data from each post it is 
    mentioned in *)
val parse : Scraper.subreddit -> stocks

(** [stock_names stocks] is the list of stock ticker symbols of all stocks in [stocks] *)
val stock_names : stocks -> string list

(** [data stock_name] is the relevant data associated with [stock] *)
val data : stocks -> string -> post list

(** [score post] is the upvotes minus downvotes of [post] *)
val upvote_score : post -> int

(** [upvote_ratio post] is the ratio of upvotes to total votes of [post] *)
val upvote_ratio : post -> float

(** [connotation post] is the connotation of language of [post] 
    The range of possible connotations is -1 to 1 from worst to best *)
val connotation : post -> float
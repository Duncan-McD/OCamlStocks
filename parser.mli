(** Representation of a Subreddit Parser *)

(** The abstract type of a stock *)
type stock

(** The abstract type of relevant data about a post *)
type post

(** [parse sub] is a list of stocks. It parses the scraped subreddit [sub] to 
    find mentioned stocks and  gets relevant data from each post it is 
    mentioned in *)
val parse : Scraper.subreddit -> stock list

(** [name stock] is the stock ticker symbol of the given [stock] *)
val name : stock -> string

(** [data stock] is the relevant data associated with [stock] for a post it 
    is mentioned in *)
val data : stock -> post list

(** [score post] is the upvotes minus downvotes of [post] *)
val score : post -> int

(** [ratio post] is the ratio of upvotes to total votes of [post] *)
val ratio : post -> float

(** [connotation post] is the connotation of language of [post] 
    The range of possible connotations is -1 to 1 from worst to best*)
val connotation : post -> float
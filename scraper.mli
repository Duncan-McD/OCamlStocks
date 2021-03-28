(** Representation of a Subreddit Scraper *)

type subreddit
(** the abstract type of a scraped subreddit *)

type post
(** The abstract type of a scraped reddit post *)

type time = Now | Today | ThisWeek | ThisMonth | ThisYear | AllTime

type subreddit_ordering = Hot | New | Rising | Top of time

exception SubredditNotFound of string
(** Raised when a subreddit is not found matching the url *)

exception TooManyPostsRequested of int
(** Raised when a user requests to parse more posts than there is in the subreddit*)

val posts : subreddit -> post list
(** [posts r] is a list of all scraped posts in subreddit [r] *)

val subreddit_name : subreddit -> string
(** [subreddit r] is the name of subreddit [r] *)

val title : post -> string
(** [title p] is the title of a post [p] *)

val body : post -> string
(** [body p] is the body of a post [p] *)

val score : post -> int
(** [upvotes p] is the score of a post [p] (upvotes - downvotes) *)

val upvote_ratio : post -> float
(** [upvote_ratio p] is the ratio of upvotes to total votes a post [p] has *)

val scrape : ?amount:int -> ?ordering:subreddit_ordering -> string -> subreddit
(** [scrape s] parses the subreddit indicated in string [s] to the 
    default 100 posts. [scrape s ~i] parses the subreddit indicated 
    in the string [s] to the requested [i] posts
    Requires: [s] is a valid url to a subreddit
    Raises [SubredditNotFound s] if there is no subreddit matching the url [s]
    Raises [TooManyPostsRequested i] if there is not enough posts in subreddit 
    to meet request.*)

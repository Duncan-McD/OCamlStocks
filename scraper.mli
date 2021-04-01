(** Representation of a Subreddit Scraper *)

(** the abstract type of a scraped subreddit *)
type subreddit

(** The abstract type of a scraped reddit post *)
type post

(** The type representing the allowed creation time of posts to scrape *)
type time = Now | Today | ThisWeek | ThisMonth | ThisYear | AllTime

(** The type representing the order of posts on reddit*)
type subreddit_ordering = Hot | New | Rising | Top of time

(** Raised when a subreddit is not found matching the url *)
exception SubredditNotFound of string

(** Raised when a user requests to parse more posts than there is in the subreddit*)
exception TooManyPostsRequested of int

(** [posts r] is a list of all scraped posts in subreddit [r] *)
val posts : subreddit -> post list

(** [subreddit r] is the name of subreddit [r] *)
val subreddit_name : subreddit -> string

(** [title p] is the title of a post [p] *)
val title : post -> string

(** [body p] is the body of a post [p] *)
val body : post -> string

(** [score p] is the score of a post [p] (upvotes - downvotes) *)
val score : post -> int

(** [upvote_ratio p] is the ratio of upvotes to total votes a post [p] has *)
val upvote_ratio : post -> float

(** [scrape s] parses the subreddit indicated in string [s] to the 
    default 100 posts and with the default New ordering on reddit
    [scrape s ~amount:i] parses the subreddit indicated in the string [s] 
    to the requested [i] posts
    [scrape s ~ordering:o] parses the subreddit in the order set by [o]
    Requires: [s] is in the form ["r/subreddit"] and [i] is positive.
    Raises [SubredditNotFound s] if there is no subreddit matching [s].
    Raises [TooManyPostsRequested i] if there is not enough posts in subreddit 
    to meet request.*)
val scrape : ?amount:int -> ?ordering:subreddit_ordering -> string -> subreddit

(** [scrape_json json] parses the [json] representing a subreddit to default all
    posts contained in [json] and with the default ordering as specified in
    [json].
    [scrape_json json ~amount:i] parses the representation of a subreddit [json]
    to the requested [i] posts
    Requires: 
        [json] is a file path that exists and is a .json file which is a 
        valid representation of a subreddit
        [i] is positive.
    Raises [TooManyPostsRequested i] if there is not enough posts in [json] 
    to meet request.*)
val scrape_json : ?amount:int -> string -> subreddit
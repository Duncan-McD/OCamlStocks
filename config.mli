(** Representation of a bot's configuration *)

(** {2 Types}*)

type t
(** The abstract data type for a program configuration *)

type subreddit = int * Scraper.subreddit_ordering * string
(** The abstract data type for an unscraped subreddit 
    (num_posts * ordering * url) *)

(** {2 Exceptions} *)

(** {2 Config Functions}*)

val default : unit -> t
(** [default] is the bot's default program configuration *)

val add_subreddit : t -> subreddit -> t
(** [add_subreddit config subreddit] is [config] with the [subreddit] added 
    to the current list of subreddit configs *)

val set_subreddits : t -> subreddit list -> t
(** [set_subreddits config subreddits] is [config] with the [subreddit] list set
    to [subreddits] *)

val set_optimizing : t -> bool -> t
(** [optimizing config b] is a [config] where optimizing is set to [b] *)

val set_consts : t -> float * float * float * float -> t
(** [set_consts config consts] is [config] with consts sets to [consts] *)

val set_tests : t -> int -> t
(** [set_tests config num_tests] is [config] with the number of tests used for 
    uniform testing set to [num_tests] *)

val set_liquidity : t -> float -> t
(** [set_liquidity config liquiditiy] is [config] with liquidity set to 
    [liquidity] *)

(* Add update config function *)

(** {2 Getter Functions}*)

val subreddit_info : t -> subreddit list
(** [subreddits_info config] is the user's [subreddit] list *)

val is_optimizing : t -> bool
(** [optimizing config] is whether or not the user wants to be optimizing *)

val consts : t -> float * float * float * float
(** [consts config] is the user's current optimization consts *)

val num_tests : t -> int
(** [num_tests config] is the amount of tests used for uniform testing *)

val liquidity : t -> float
(** [liquidity config] is the liquidity in the user's portfolio  *)

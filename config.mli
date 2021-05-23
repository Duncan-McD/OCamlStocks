(** Representation of a bot's configuration *)

(** {2 Types}*)

type t
(** The abstract data type for a program configuration *)

(** {2 Config Functions}*)

val default : t
(** [default] is the bot's default program configuration *)

(** {2 Config Functions}*)

(* Add update config function *)

(** {2 Getter Functions}*)

(* Add get config values function *)

val number_of_tests : t -> int

val subreddit_list : t -> string list

val posts_per_scrape : t -> int

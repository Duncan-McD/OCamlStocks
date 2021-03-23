type t

type post

val posts : t -> post list

val subreddit : t -> string

(* getters *)
val title : post -> string
val body : post -> string

(* scrapes subreddit w/ cutoff -> t *)
val scrape_default : string -> t
val scape_amount : string -> int -> t
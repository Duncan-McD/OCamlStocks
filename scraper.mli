type t

type post

val posts : t -> post list

val subreddit : t -> string

(* getters *)
val title : post -> string
val body : post -> string

(* scrapes subreddit w/ optional cutoff -> t *)
val scrape_default : amount:int -> string -> t
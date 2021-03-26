type post = { post_name : string; body : string }

type subreddit = { subreddit_name : string; posts : post list }

exception SubredditNotFound of string

exception TooManyPostsRequested of int

let posts (scraper : subreddit) : post list = failwith "unimplemented"

let subreddit_name (scraper : subreddit) : string = failwith "unimplemented"

let title (post : post) : string = failwith "unimplemented"

let body (post : post) : string = failwith "unimplemented"

let scrape ?(amount = 100) (subreddit : string) : subreddit =
  failwith "unimplemented"

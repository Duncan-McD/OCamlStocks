open Mechaml
open Yojson
module U = Yojson.Basic.Util
open Yojson.Basic.Util
module M = Mechaml.Agent.Monad
open M.Infix
open Soup

(** [get_soup site] is the soup node representation of website [site] *)
let get_soup (site : string) : soup node M.m =
  Mechaml.Agent.get site >|= Mechaml.Agent.HttpResponse.page
  >|= Mechaml.Page.soup

(** [get_full_page_json soup_node] is the json representation of [soup_node] *)
let get_full_page_json (soup_node : soup node M.m) : Yojson.Basic.t =
  match M.run (Mechaml.Agent.init ()) soup_node with
  | _, node -> node |> texts |> List.hd |> Yojson.Basic.from_string

type post = {
  post_name : string;
  body : string;
  score : int;
  upvote_ratio : float;
}

type time = Now | Today | ThisWeek | ThisMonth | ThisYear | AllTime

type subreddit_ordering = Hot | New | Rising | Top of time

type subreddit = { subreddit_name : string; posts : post list }

exception SubredditNotFound of string

exception TooManyPostsRequested of int

exception ExitLoop of int

let posts (subreddit : subreddit) : post list = subreddit.posts

let subreddit_name (subreddit : subreddit) : string = subreddit.subreddit_name

let title (post : post) : string = post.post_name

let body (post : post) : string = post.body

let score (post : post) : int = post.score

let upvote_ratio (post : post) : float = post.upvote_ratio

(** [get_time_url_parameter time] is the query string required to request Top
    posts according to [time] *)
let get_time_url_parameter (time : time) : string =
  match time with
  | Now -> "&t=hour"
  | Today -> "&t=day"
  | ThisWeek -> "&t=week"
  | ThisMonth -> "&t=month"
  | ThisYear -> "&t=year"
  | AllTime -> "&t=all"

(** [get_ordering_url_parameter order] is the path required to order posts
    according to [order] *)
let get_ordering_url_parameter (ordering : subreddit_ordering) : string =
  match ordering with
  | Hot -> "/hot.json?limit=100"
  | New -> "/new.json?limit=100"
  | Rising -> "/rising.json?limit=100"
  | Top time -> "/top.json?limit=100" ^ get_time_url_parameter time

(** [resolve_after_string_option s] is [str] if [s] is [Some str], is "null"
    otherwise *)
let resolve_after_string_option (string_option : string option) : string =
  match string_option with Some str -> str | None -> "null"

(** [index_of_substring str sub] is the index of the first occurance of [sub] in
    [str], is [-1] if [sub] is not a substring of [str] *)
let index_of_substring (s : string) (sub : string) : int =
  try
    let sub_length = String.length sub in
    for i = 0 to String.length s - sub_length do
      if String.sub s i sub_length = sub then raise (ExitLoop i)
    done;
    -1
  with ExitLoop i -> i

(** [replace_after_parameter s param] is the URL of the next batch of posts
    after given post id [param] of subreddit [s] *)
let replace_after_parameter (subreddit : string) after_parameter : string =
  let i = index_of_substring subreddit "&after=" in
  if i = -1 then subreddit ^ "&after=" ^ after_parameter
  else
    let subreddit_without_after = String.sub subreddit 0 i in
    subreddit_without_after ^ "&after=" ^ after_parameter

(** [get_json s] is the json representation of subreddit [s] *)
let get_json (subreddit : string) : Yojson.Basic.t =
  get_full_page_json (get_soup subreddit)

(** [scrape_json_name json] is the name of the json representation of a
    subreddit [json] *)
let scrape_json_name (json : Yojson.Basic.t) : string =
  json |> member "data" |> member "children" |> U.to_list |> List.hd
  |> member "data"
  |> member "subreddit_name_prefixed"
  |> U.to_string

(** [scrape_subreddit_name s] is the name of subreddit [s] *)
let scrape_subreddit_name (subreddit : string) : string =
  subreddit |> get_json |> scrape_json_name

(** [get_after_id s] is the after paramter (for the next batch of posts) of
    subreddit [s] *)
let get_after_id (subreddit : string) : string =
  subreddit |> get_json |> member "data" |> member "after" |> U.to_string_option
  |> resolve_after_string_option

(** [get_post_list json] is the list of posts in the json representation of a
    subreddit [json] *)
let get_post_list (json : Yojson.Basic.t) : Yojson.Basic.t list =
  json |> member "data" |> member "children" |> U.to_list

(** [get_json_post_list s] is the list of posts of a subreddit [s] *)
let get_json_post_list (subreddit : string) : Yojson.Basic.t list =
  subreddit |> get_json |> get_post_list

(** [build_post p] post is the post representation of json post [p] *)
let build_post post =
  {
    post_name = post |> member "data" |> member "title" |> U.to_string;
    body = post |> member "data" |> member "selftext" |> U.to_string;
    score = post |> member "data" |> member "score" |> U.to_int;
    upvote_ratio = post |> member "data" |> member "upvote_ratio" |> U.to_number;
  }

(** [scrape_posts sub amount param curr json] is the post list representation of
    subreddit [sub] with amount [amount] *)
let rec scrape_posts (subreddit : string) (amount : int)
    (after_parameter : string) (current_posts : post list)
    (json_post_list : Yojson.Basic.t list) : post list =
  match json_post_list with
  | h :: t ->
      let post = build_post h in
      let new_posts_list = post :: current_posts in
      if amount = 1 then new_posts_list
      else scrape_posts subreddit (amount - 1) after_parameter new_posts_list t
  | [] ->
      if after_parameter = "null" then raise (TooManyPostsRequested amount);
      let new_subreddit_link =
        replace_after_parameter subreddit after_parameter
      in
      scrape_posts new_subreddit_link amount
        (get_after_id new_subreddit_link)
        current_posts
        (get_json_post_list new_subreddit_link)

(** [subreddit_doesnt_exist s] returns true if subreddit [s] has no posts *)
let subreddit_doesnt_exist (subreddit : string) : bool =
  "https://reddit.com/" ^ subreddit ^ ".json"
  |> get_json |> member "data" |> member "dist" |> to_int = 0

let scrape ?(amount = 100) ?(ordering = New) (subreddit : string) : subreddit =
  if subreddit_doesnt_exist subreddit then raise (SubredditNotFound subreddit);
  let new_subreddit_link =
    "https://www.reddit.com/" ^ subreddit ^ get_ordering_url_parameter ordering
  in
  {
    subreddit_name =
      scrape_subreddit_name ("https://www.reddit.com/" ^ subreddit ^ ".json");
    posts =
      scrape_posts new_subreddit_link amount
        (get_after_id new_subreddit_link)
        []
        (get_json_post_list new_subreddit_link);
  }

let scrape_json ?(amount = 100) (json_file : string) : subreddit =
  let json = Yojson.Basic.from_file json_file in
  {
    subreddit_name = scrape_json_name json;
    posts = scrape_posts "" amount "null" [] (get_post_list json);
  }

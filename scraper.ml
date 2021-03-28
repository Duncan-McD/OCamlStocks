open Mechaml
open Yojson
module U = Yojson.Basic.Util
open Yojson.Basic.Util
module M = Mechaml.Agent.Monad
open M.Infix
open Soup
module Req = Soup.R

let get_soup (site : string) : soup node M.m =
  Mechaml.Agent.get site >|= Mechaml.Agent.HttpResponse.page
  >|= Mechaml.Page.soup

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

let get_time_url_parameter (time : time) : string =
  match time with
  | Now -> "&t=hour"
  | Today -> "&t=day"
  | ThisWeek -> "&t=week"
  | ThisMonth -> "&t=month"
  | ThisYear -> "&t=year"
  | AllTime -> "&t=all"

let get_ordering_url_parameter (ordering : subreddit_ordering) : string =
  match ordering with
  | Hot -> "/hot.json?limit=100"
  | New -> "/new.json?limit=2"
  | Rising -> "/rising.json?limit=100"
  | Top time -> "/top.json?limit=100" ^ get_time_url_parameter time

let reslove_after_string_option string_option =
  match string_option with Some str -> str | None -> "null"

let index_of_substring (s : string) (sub : string) : int =
  try
    let sub_length = String.length sub in
    for i = 0 to String.length s - sub_length do
      if String.sub s i sub_length = sub then raise (ExitLoop i)
    done;
    -1
  with ExitLoop i -> i

let replace_after_parameter (subreddit : string) after_parameter : string =
  let i = index_of_substring subreddit "&after=" in
  if i = -1 then subreddit ^ "&after=" ^ after_parameter
  else
    let subreddit_without_after = String.sub subreddit 0 i in
    subreddit_without_after ^ "&after=" ^ after_parameter

let get_json (subreddit : string) : Yojson.Basic.t =
  get_full_page_json (get_soup subreddit)

let scrape_subreddit_name (subreddit : string) : string =
  subreddit |> get_json |> member "data" |> member "children" |> U.to_list
  |> List.hd |> member "data"
  |> member "subreddit_name_prefixed"
  |> U.to_string

(** [get_after_id s] is the after paramter from the json *)
let get_after_id (subreddit : string) : string =
  subreddit |> get_json |> member "data" |> member "after" |> U.to_string_option
  |> reslove_after_string_option

let get_json_post_list (subreddit : string) : Yojson.Basic.t list =
  subreddit |> get_json |> member "data" |> member "children" |> U.to_list

let rec scrape_posts (subreddit : string) (amount : int)
    (after_parameter : string) (current_posts : post list)
    (json_post_list : Yojson.Basic.t list) : post list =
  match json_post_list with
  | h :: t ->
      let post =
        {
          post_name = h |> member "data" |> member "title" |> U.to_string;
          body = h |> member "data" |> member "selftext" |> U.to_string;
          score = h |> member "data" |> member "score" |> U.to_int;
          upvote_ratio =
            h |> member "data" |> member "upvote_ratio" |> U.to_float;
        }
      in
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

let checkthis () = scrape "r/wallstreetbets" ~amount:8

let testYojsonAfter () =
  "https://www.reddit.com/r/wallstreetbets.json" |> get_json |> member "data"
  |> member "after" |> U.to_string_option

let testYojsonBefore () =
  "https://www.reddit.com/r/wallstreetbets.json" |> get_json |> member "data"
  |> member "before" |> U.to_string_option

let checkEmptyCommunity () = scrape "r/testingEmptyCommunity" ~amount:3

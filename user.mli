(** This module represents a user of our program*)

type t
(** The abstract data type of a user*)

val create : string -> string -> string -> t
(** [create e n p] is a new user account with email e name n and password p*)

val email : t -> string
(** [email u] is the email of user u*)

val name : t -> string
(** [name u] is the full name of user u*)

val last_name : t -> string
(** [name u] is the last name of user u if first and last was provided, 
    otherwise it is the one name they entered*)

val first_name : t -> string
(** [name u] is the first name of user u if first and last was provided, 
    otherwise it is the one name they entered*)

val password : t -> string
(** [password u] is the password of user u*)

val current_portfolio : t -> Portfolio.t
(** [current_portfolio u] is the current portfolio of the user u*)

val past_portfolios : t -> Portfolio.t list
(** [past_portfolios u] is the list of all past portfolios of user u*)

val test_portfolios : t -> Portfolio.t list
(** [test_portfolios u] is the list of test portfolios for user u*)

val config : t -> Config.t
(** [config u] is the program configuration for user u*)

val change_config : t -> Config.t -> t
(** [change_config u c] changes the configuration of user u to config c*)

val update_portfolio : t -> Portfolio.t -> t
(** [update_portfolio u p] adds user u's old current portfolio to past 
    portfolios and sets the new current portfolio to portfolio p*)

val change_test_portfolios : t -> Portfolio.t list -> t
(** [change_test_portfolios u t] changes the user u's test portfolios to the 
    portfolio list t*)

val to_json_string : t -> string

val user_of_json : Yojson.Basic.t -> t

val set_last_daily_task_timestamp : t -> float -> t
(** [set_last_daily_task_timestamp u t] sets the time of the last daily tasks 
    timestamp of user u to time *)

val last_daily_task_timestamp : t -> float
(** [last_daily_task_timestamp u] is the last time the user u did daily
    tasks. This time is the amount of seconds since 00:00:00 GMT, Jan. 1, 1970*)

val time_for_daily_tasks : t -> float -> bool
(** [time_for_daily_tasks u t] is true if it has been at least a day since the 
    last time daily tasks were run. Otherwise it is false*)

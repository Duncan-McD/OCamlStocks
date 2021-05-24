(** Representation of a program state *)

(** {2 Types}*)

type action

type t
(** The abstract data type for a state *)

(** {2 Exceptions} *)

exception InvalidAction of string
(** Raised when an string cannot be converted to an [action]*)

exception QuitAction
(** Raised when a user wants to quit the program *)

exception LogoutAction
(** Raised when a user wants to logout of their account *)

(** {2 Action Functions} *)

val action_of_string : string -> action
(** [action_of_string s] is an [action] according to [s] 
    Raises [InvalidAction s] if [s] cannot be converted into an [action].
    Raises [QuitAction] if [s] is "quit".
*)

(** {2 State Functions} *)

val init : Auth.auth -> User.t -> t
(** [init auth user] is the program state *)

val update : t -> action -> unit
(** [update state action] updates [state] based on [action] *)

(** {2 Getter Functions}*)

val user : t -> User.t
(** [user state] is the current user data *)

val config : t -> Config.t
(** [list_of_stocks p] is the user's configuration settings *)

val portfolio : t -> Portfolio.t
(** [portfolio state] is the user's portfolio *)

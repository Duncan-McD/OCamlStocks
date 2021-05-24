(** Representation of a program state *)

(** {2 Types}*)

type action

type t = { auth : Auth.auth; mutable user : User.t; mutable state : action }
(** The abstract data type for a state *)

(** {2 Exceptions} *)

exception InvalidAction of (string * string)
(** Raised when an string cannot be converted to an [action]*)

exception InapplicableAction of (string * string)
(** Raised when a string's coorisponding action cannot be called from the 
    location you are in*)

exception QuitAction
(** Raised when a user wants to quit the program *)

exception LogoutAction
(** Raised when a user wants to logout of their account *)

exception HelpAction
(** Raised when a user wants to see program help information *)

(** {2 Action Functions} *)

val action_of_string : t -> string -> action
(** [action_of_string st s] is an [action] according to [s] and [st]
    Raises [InvalidAction s] if [s] cannot be converted into an [action].
    Raises [InapplicableAction] if [s] cannot be called in the given [st].
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

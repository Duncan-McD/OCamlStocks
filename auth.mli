(** Representation of retrieving and authenticating a valid user *)

(** {2 Types}*)

type t
(** The abstract data type for auth *)

type prompt =
  | Initial_Prompt
  | Logged_Out
  | Invalid_Input
  | Empty_Input
  | Already_Taken
  | Changed  (** The type of prompts *)

type auth = Login | Signup  (** The type of the authentication used *)

(** {2 Excepions} *)

exception QuitException

(** {2 Auth Functions} *)

val prompt_user : prompt -> t
(** [prompt_user prompt] is an auth type retreived by prompting the user
    to login / signup and then asking for their credentials *)

(** {2 Getter Functions}*)

val email : t -> string
(** [list_of_stocks p] is the user's configuration settings *)

val auth_type : t -> auth

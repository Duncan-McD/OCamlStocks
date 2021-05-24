(** Representation of retrieving and authenticating a valid user *)

(** {2 Types}*)

type t
(** The abstract type of auth  *)

type prompt =
  | Initial_Prompt
  | Logged_Out
  | Invalid_Input
  | Empty_Input
  | Already_Taken
  | Changed  (** The type of prompts *)

type auth = Login | Signup  (** The type of auth used to get the user *)

(** {2 Excepions} *)

exception QuitException

(** {2 Auth Functions} *)

val prompt_user : prompt -> t
(** [prompt_user prompt] is the user retreived by prompting the user
    to login / signup and then asking for their credentials *)

val auth_type : t -> auth
(** [auth_type auth] is the [auth] the user used *)

val user : t -> User.t
(** [user auth] is the [user] *)

(** {2 Getters Functions} *)

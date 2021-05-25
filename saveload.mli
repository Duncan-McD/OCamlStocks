val file : string
(** [file] is the file where all data is saved to and loaded from*)

val save_user : User.t -> unit
(**[save_user u] saves the data of user [u] to [file]*)

val is_valid_email_password : string -> string -> bool
(**[is_valid_email_password e p] is true if there currently exists user with 
  email [e] and password [p], and is false otherwise*)

val load_user : string -> User.t
(**[load_user e] is user with email [e]. Precondition: [e] must be an email 
of a user*)

val delete_user : User.t -> unit
(**[delete_user u] deletes the data of user [u] from [file]*)

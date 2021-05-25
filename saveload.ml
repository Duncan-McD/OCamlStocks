open Yojson.Basic.Util

let file = "data.json"

(**[write_empty_file ()] replaces the saving and loading file with
 an empty file*)
let write_empty_file () =
  let oc = open_out file in
  Printf.fprintf oc "%s\n" "";
  close_out oc

(**[load_user_json_list ()] is the json list of users loaded from file*)
let load_user_json_list () =
  try to_list (Yojson.Basic.from_file file) with
  | Sys_error s ->
      write_empty_file ();
      []
  | Yojson.Json_error e -> []

(**[update_user_json_list u l acc] is the json list of users with the
 previous json of [u] replaced with the [u]*)
let rec update_user_json_list (user : User.t)
    (user_json_list : Yojson.Basic.t list) (acc : Yojson.Basic.t list) =
  match user_json_list with
  | [] -> User.to_json user :: acc
  | (h : Yojson.Basic.t) :: (t : Yojson.Basic.t list) ->
      if to_string (member "name" h) = User.name user then
        (User.to_json user :: acc) @ t
      else update_user_json_list user t (h :: acc)

(**[update_user_json_list u l acc] is the json list of users with the
json of [u] removed*)
let rec remove_user_json_list (user : User.t)
    (user_json_list : Yojson.Basic.t list) (acc : Yojson.Basic.t list) =
  match user_json_list with
  | [] -> acc
  | (h : Yojson.Basic.t) :: (t : Yojson.Basic.t list) ->
      if to_string (member "name" h) = User.name user then acc @ t
      else update_user_json_list user t (h :: acc)

let save_user (user : User.t) =
  let user_json_list = update_user_json_list user (load_user_json_list ()) [] in
  let json_of_json_list = `List user_json_list in
  Yojson.Basic.to_file file json_of_json_list

let delete_user (user : User.t) =
  let user_json_list = remove_user_json_list user (load_user_json_list ()) [] in
  let json_of_json_list = `List user_json_list in
  Yojson.Basic.to_file file json_of_json_list

(**[check_email_password l e p] is true if a user in user json list [l] has
   an email [e] and a password [p], false otherwise*)
let rec check_email_password user_json_list email password =
  match user_json_list with
  | [] -> false
  | h :: t ->
      if
        to_string (member "email" h) = email
        && to_string (member "password" h) = password
      then true
      else check_email_password t email password

let is_valid_email_password (email : string) (password : string) =
  check_email_password (load_user_json_list ()) email password

(**[get_user_from_user_json_list l e ] is the json of the user with email [e].
 Precondition: [e] must be the email of a user in json list [l]*)
let rec get_user_from_user_json_list user_json_list email =
  match user_json_list with
  | [] -> failwith "Email not found"
  | h :: t ->
      if to_string (member "email" h) = email then (
        print_endline (to_string (member "email" h));
        h)
      else get_user_from_user_json_list t email

let load_user email =
  User.user_of_json
    (get_user_from_user_json_list (load_user_json_list ()) email)

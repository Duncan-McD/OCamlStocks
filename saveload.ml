open Yojson.Basic.Util

let file = "data.json"

let write_empty_file =
  let oc = open_out file in
  Printf.fprintf oc "%s\n" "";
  close_out oc

let load_user_json_list =
  try to_list (member "user" (Yojson.Basic.from_file file))
  with Sys_error s ->
    write_empty_file;
    []

let rec update_user_json_list (user : User.t)
    (user_json_list : Yojson.Basic.t list) (acc : Yojson.Basic.t list) =
  match user_json_list with
  | [] -> Yojson.Basic.from_string (User.to_json_string user) :: acc
  | (h : Yojson.Basic.t) :: (t : Yojson.Basic.t list) ->
      if to_string (member "name" h) = User.name user then
        (Yojson.Basic.from_string (User.to_json_string user) :: acc) @ t
      else update_user_json_list user t (h :: acc)

let save_user (user : User.t) =
  let user_json_list = update_user_json_list user load_user_json_list [] in
  let json_of_json_list = `List user_json_list in
  Yojson.Basic.to_file file json_of_json_list

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
  check_email_password load_user_json_list email password

let rec get_user_from_user_json_list user_json_list email =
  match user_json_list with
  | [] -> failwith "Email not found"
  | h :: t ->
      if to_string (member "email" h) = email then h
      else get_user_from_user_json_list t email

let load_user email =
  User.user_of_json (get_user_from_user_json_list load_user_json_list email)

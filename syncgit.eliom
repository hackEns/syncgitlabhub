{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
  open Html5.F
}}

open Syncgitcore
open Syncgitpush
open Lwt

module Syncgit_app =
  Eliom_registration.App (
    struct
      let application_name = "syncgit"
    end)

open Eliom_tools.F

let send_error str =
	Ocsigen_messages.errlog str;
	Lwt.return
        (html ~title:"error" (body [pcdata ("Error: " ^ str)]))

let main_service_handler target_repo_opt post_args =
	try
		match post_args with
		| _, (Some c) -> 
			begin
			(* FIXME: is 10000 ok for message length? *)
			(Ocsigen_stream.string_of_stream 10000 (Ocsigen_stream.get c)) >>= fun (str) ->
				let js = Yojson.Safe.from_string str in
				match js with
				| `Assoc(args_list) ->
					let repo_state = parse_json_arguments_list args_list in
					let target_repository = do_sync repo_state target_repo_opt in
					let _ = Ocsigen_messages.accesslog ("Has synced " ^ (args_str (repo_state.repository.name))
						^ " " ^ (args_str (repo_state.repository.source_http_url)) ^ " with " ^ target_repository) in
					Lwt.return
						(html
						~title:"syncgit"
						(body [
							pcdata "Has synced ";
							pcdata (args_str (repo_state.repository.name));
							pcdata " with ";
							pcdata target_repository;
							pcdata ".";
							]))
				| _ ->
					raise BadArgumentsError
			end
		| _ -> raise BadArgumentsError
	with
		| BadArgumentsError ->
			send_error "Data not sent in a supported format, probably not JSON."
		| NotEnoughArgumentsError(a) ->
			send_error ("Not enough arguments in the POST request: " ^ a ^ " missing")
		| NotAuthorizedError ->
			send_error "Not authorized to sync this repository."
		| InvalidArgument(a) ->
			send_error ("Invalid argument: " ^ a ^ " missing or invalid")
		| UnknownGitPushError ->
			send_error ("Git push error.")
		| UnknownGitPullError ->
			send_error ("Git pull error.")
		| UnknownGitCloneError ->
			send_error ("Git clone error.")



let _ =
    let api_no_post = Syncgit_app.register_service
        ~path:[]
        ~get_params:Eliom_parameter.(suffix (opt (string "user" ** string "repo")))
        (fun v () -> Lwt.return
            (html
                ~title:"syncgit"
                (body [pcdata "Post requests only"])))
    in

    Syncgit_app.register_post_service
        ~fallback:api_no_post
        ~post_params:Eliom_parameter.(raw_post_data)
        (function
			| Some (user_get, repo_get) ->
				let target_repo = Some (user_get ^ "/" ^ repo_get) in
				main_service_handler target_repo
			| None -> main_service_handler None
		)

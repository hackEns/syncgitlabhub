open Syncgitcore
open Thread
open Unix
open Lwt

let do_sync repo_state target_repository_opt = 
	let target_repository = 
		match target_repository_opt with
		| Some t -> t
		| None ->
			begin
			match repo_state.repository.name with
			| Some s -> Syncgitconfig.github_user ^ "/" ^ s
			| None -> raise (InvalidArgument "source http url")
			end
	in
	match repo_state.repository.name, repo_state.repository.source_http_url with
	| Some name, Some source_http_url ->
		begin
		print_string ("Syncing " ^ name);
		let (valid, end_source_url) = is_valid_uri source_http_url in
		if valid then
		let project_path = Syncgitconfig.path_git ^ "/" ^ end_source_url in
		  begin
		  	begin
			try
				begin
				Unix.stat project_path;
				Unix.chdir project_path;
				let WEXITED i = Lwt_main.run (Lwt_process.exec ("git", [|"git";"pull";source_http_url|])) in
				if i = 0 then ()
				else raise UnknownGitPullError
				end
			with
				| Unix.Unix_error(_) ->
					begin
					ensure_path_exist Syncgitconfig.path_git;
					(* Now we can check if it has already been cloned *)
					let WEXITED i = Lwt_main.run (Lwt_process.exec ("git", [|"git";"clone";source_http_url;project_path|])) in
					if i = 0 then ()
					else raise UnknownGitCloneError
					end
			end;

			Unix.chdir project_path;
			let WEXITED i = Lwt_main.run (Lwt_process.exec ("git", [|"git";"push";Syncgitconfig.github_url ^ target_repository;"master"|])) in
			if i == 0 then
				target_repository
			else
				raise UnknownGitPushError
		  end
		else
			raise (InvalidArgument "source http url")
		end
	| _ -> raise (NotEnoughArgumentsError "name, git source")

let launch_sync r =
	Thread.create do_sync r


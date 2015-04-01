open Syncgitcore
open Thread
open Unix
open Lwt

let do_sync repo_state = 
	match repo_state.repository.name, repo_state.repository.source_http_url, repo_state.repository.target_ssh_url with
	| Some name, Some source_http_url, Some target_ssh_url ->
		begin
		print_string ("Syncing " ^ name);
		let WEXITED i = Lwt_main.run (Lwt_process.exec ("git", [|"git";"clone";source_http_url;"/tmp/git-repo"|])) in
		if i == 0 then
			let WEXITED i = Lwt_main.run (Lwt_process.exec ("git", [|"git";"push";target_ssh_url;|])) in
			if i == 0 then
				()
			else
				raise UnknownGitError
		else
			raise UnknownGitError
		end
	| _ -> raise (NotEnoughArgumentsError "name, git source, git target")

let launch_sync r =
	Thread.create do_sync r


let _ =
	let r_ = { repository_empty with source_http_url = Some "https://git.eleves.ens.fr/hackens/PassManager.git"; name = Some "PassManager"; target_ssh_url = Some "/tmp/testgit"; } in
	let r = { repository_state_empty with repository = r_; } in
	launch_sync r; while true do Format.printf "test@."; Unix.sleep 1;done;

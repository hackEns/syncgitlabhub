exception BadArgumentsError
exception NotAuthorizedError
exception NotEnoughArgumentsError of string
exception UnknownGitError

type author = { name:string; email:string; }

type commit = { id: string; message: string; url: string; author: author; }

type repository = {
	source_http_url: string option;
	source_ssh_url: string option;
	name:string option;
	target_ssh_url: string option;
	}

type repository_state = {
	repository: repository;
	commits: commit list;
}

let repository_empty = { source_http_url = None; source_ssh_url = None; name = None; target_ssh_url = None }
let repository_state_empty = {
	repository = repository_empty;
	commits = [];
}

let rec parse_json_repository = function
	| [] -> repository_empty
	| t::q ->
		let r = parse_json_repository q in
		match t with
			| ("name", `String a) -> { r with name = Some a; }
			| ("git_http_url", `String a) -> { r with source_http_url = Some a; }
			| ("git_ssh_url", `String a) -> { r with source_ssh_url = Some a; }
			| _ -> r

(** Browse the association list (got from a YoJson `Assoc) to return a repository_state structure. *)
let rec parse_json_arguments_list = function
	| [] -> repository_state_empty
	| t::q ->
		let args = parse_json_arguments_list q in
		match t with
			| ("repository", `Assoc(l)) -> { args with repository = parse_json_repository l; }
			| _ -> args



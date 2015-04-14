(* (c) Syncgit - Lucas Baudin <lucas.baudin@ens.fr> – 2015 - see LICENSE file. *)
exception BadArgumentsError
exception NotAuthorizedError
exception NotEnoughArgumentsError of string
exception UnknownGitError
exception UnknownGitCloneError
exception UnknownGitPullError
exception UnknownGitPushError
exception InvalidArgument of string 

(** These first two records are not used yet. *)
type author = { name:string; email:string; }

type commit = { id: string; message: string; url: string; author: author; }

type repository = {
	(** Used to clone the repository. *)
	source_http_url: string option;
	(** Not used yet, but should be an alternative to http if the future. *)
	source_ssh_url: string option;
	(** Name of the repository, for instance "PassManager" *)
	name:string option;
	}

(** This record is used when a POST request is received: most arguments directly get to it. *)
type repository_state = {
	repository: repository;
	commits: commit list;
}

let repository_empty = { source_http_url = None; source_ssh_url = None; name = None; }

let repository_state_empty = {
	repository = repository_empty;
	commits = [];
}

(** This must only be used when the first argument is surely a Some and not a
    None (if it is not the first time it is used). For concision only. *)
let args_str = function
	| Some s -> s
	| None -> raise (InvalidArgument "")

(** Takes a YoJson record, returns the corresponding repository record.
    See the gitlab reference web api, this thing corresponds to the
	repository subdictionary.*)
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



(* Some functions to manipulate URI *)


(** Apply f increasingly. For instance, for a path /home/user/test,
    it is called on /home, /home/user, /home/user/test *)
let split_char_increasing sep str f =
	let rec sp_aux str old_str =
		try
			let i = String.index str sep in
			let new_str = old_str ^ (String.sub str 0 (i+1)) in
			let a = f new_str in
			a ::
			sp_aux (String.sub str (i+1) (String.length str - i - 1)) (new_str)
		with Not_found ->
			let new_str = old_str ^ str in
			if str = "" then
				[]
			else
				[f new_str]
	in sp_aux str ""

(** Check wether the clone url is whitelisted. *)
let ensure_can_clone s =
	let prefix = Syncgitconfig.source_prefix in
	let l = String.length prefix in
	String.sub s 0 l = prefix

(** Return (true, end_of_uri), or (false, "") if it is not a valid uri *)
let is_valid_uri s =
	let length = String.length s in
	if length < 8 then
		(false, "")
	else
		if String.sub s 0 8 = "https://" then
			(true && ensure_can_clone s, String.sub s 8 (length - 8))
		else if String.sub s 0 7 = "http://" then
			(true && ensure_can_clone s, String.sub s 7 (length - 7))
		else
			(false, "")

(** If we know that path doesn't exist, this function can be used to create
    path and every parent of it if needed. Poor permissions management, use
	it with caution. Throws some Unix_error if it can't be created. *)
let ensure_path_exist path =
	let _ = split_char_increasing '/' path (fun s ->
		try
			begin
			Unix.mkdir s 0o740;
			end
		with
			Unix.Unix_error(Unix.EEXIST, _, _) -> ()) in ()


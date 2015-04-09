exception BadArgumentsError
exception NotAuthorizedError
exception NotEnoughArgumentsError of string
exception UnknownGitError
exception InvalidArgument of string 

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



(* Some functions to manipulate URI *)


(** Apply f increasingly. For instance, for a path /home/user/test, it is called on /home, /home/user, /home/user/test *)
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

(** Return (true, end_of_uri), or (false, "") if it is not a valid uri *)
let is_valid_uri s =
	let length = String.length s in
	if length < 8 then
		(false, "")
	else
		if String.sub s 0 8 = "https://" then
			(true, String.sub s 8 (length - 8))
		else if String.sub s 0 7 = "http://" then
			(true, String.sub s 8 (length - 8))
		else
			(false, "")

let ensure_path_exist path =
	let _ = split_char_increasing '/' path (fun s ->
		try
			begin
			Unix.mkdir s 0o740;
			end
		with
			Unix.Unix_error(Unix.EEXIST, _, _) -> ()) in ()

let _ = ensure_path_exist "/tmp/test/re-test/rere-test/"
	

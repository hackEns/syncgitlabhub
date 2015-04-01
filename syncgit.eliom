{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
  open Html5.F
}}

open Syncgitcore
open Lwt

module Syncgit_app =
  Eliom_registration.App (
    struct
      let application_name = "syncgit"
    end)

open Eliom_tools.F
(*let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()*)

let send_error str =
    Lwt.return
        (html ~title:"error" (body [pcdata ("Error: " ^ str)]))



let _ =
    let api_no_post = Syncgit_app.register_service
        ~path:[]
        ~get_params:Eliom_parameter.(suffix (string "user" ** string "repo"))
        (fun v () -> Lwt.return
            (html
                ~title:"syncgit"
                (body [pcdata "Post requests only"])))
    in

    Syncgit_app.register_post_service
        ~fallback:api_no_post
        ~post_params:Eliom_parameter.(raw_post_data)
        (fun (user_get, repo_get) post_args -> 
            try
                match post_args with
                    | _, (Some c) -> 
                        begin
                        (* FIXME: is 10000 ok for message length? *)
                        (Ocsigen_stream.string_of_stream 10000 (Ocsigen_stream.get c)) >>= fun (str) ->
                            let js = Yojson.Safe.from_string str in
                            match js with
                            (*| `List(t::q) -> 
                                Lwt.return
                                (html
                                ~title:"syncgit"
                                (body [pcdata (Yojson.Safe.to_string (t)); pcdata "list"]))*)
                            | `Assoc(args_list) ->
								let repo_state = parse_json_arguments_list args_list in
								begin
								match repo_state.repository.name with
									| Some name ->
										Lwt.return
										(html
										~title:"syncgit"
										(body [pcdata "Must sync "; pcdata name; pcdata "with "; pcdata (user_get ^ "/" ^ repo_get);]))
									| _ -> raise (NotEnoughArgumentsError "name")
								end
                            | _ ->
                                raise BadArgumentsError
                    end
                    | _ -> raise BadArgumentsError
            with
                | BadArgumentsError -> send_error "Data not sent in a supported format, probably not JSON."
                | NotEnoughArgumentsError(a) -> send_error ("Not enough arguments in the POST request: " ^ a ^ " missing")
                | NotAuthorizedError -> send_error "Not authorized to sync this repository."
                | InvalidArgument(a) -> send_error ("Invalid argument: " ^ a ^ " missing")

        )

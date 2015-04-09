open Config_file

let conf = new group
let path_git_ = new string_cp ~group:conf ["path_git"] "/tmp/git" "Path where we can store the git repositories. It must exist with the appropriate permission."
let github_user_ = new string_cp ~group:conf ["github_user"] "hackens" "GitHub user"
let github_url_ = new string_cp ~group:conf ["github_url"] "git@github.com:" "GitHub URL"

let _ = conf#read "syncgit.config"

let path_git = path_git_#get
let github_user = github_user_#get
let github_url = github_url_#get

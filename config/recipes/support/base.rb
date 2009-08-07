set :scm,         :git
set :scm_verbose, true

set(:branch) { Capistrano::CLI.ui.ask "What do you want to release? (i.e. if production, give a tag, if staging type 'staging'" }

set :deploy_via,    :remote_cache

set :database_name, "cellucidate"
set :database_user, "cellucidator"

set :use_sudo,      false
set :user,          'root'
set :keep_releases, 3

ssh_options[:paranoid] = false
ssh_options[:username] = 'root'
ssh_options[:keys] = ENV['AWS_SSHKEY']

namespace :deploy do

  task :setup_var_app do
    run "mkdir -p /var/app"
  end

end

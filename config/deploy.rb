require 'active_support'
require 'config/management'
load File.expand_path(File.dirname(__FILE__) + '/recipes/support/group.rb')
load File.expand_path(File.dirname(__FILE__) + '/recipes/support/base.rb')
load File.expand_path(File.dirname(__FILE__) + '/recipes/support/config.rb')
load File.expand_path(File.dirname(__FILE__) + '/recipes/support/s3util.rb')

set :application, "plx_engine"
set :deploy_to,   "/var/app/plx_engine"
set :repository,  "git@git.plectix.com:plx_engine.git"

namespace :deploy do

  task :update_revision do
    run "echo '#{branch}' >> #{current_path}/REVISION"
  end

  task :restart do
  end

  task :stop do
  end

  task :start do
  end

  task :compile do
    run "cd #{release_path} && make" do |ch,stream,text|
      #ch[:state] ||= {}
      #ch.send_data("t\n")
    end
  end

end

#
# ===================================================================
before "deploy:setup",   "deploy:setup_var_app"

after "deploy",  "deploy:compile"
after "deploy",  "deploy:update_revision"


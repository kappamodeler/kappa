#
# Deploy recipe for plx_engines
#

namespace :release do
  namespace :plx_engine do

    # Kickstart for releasing plx-engine
    task :default do
      release.plx_engine.update_code
      release.plx_engine.compile
    end
    # compilation
    task :compile, :roles => :job do
      run "cd /var/app/plx_engine/current; make;"
    end

    task :update_code do
      set :repository, "https://svn.plectix.com/plectix/plx-engine/#{repository_path}"
      set :application, "plx_engine"
      release.reset_paths
      run "svn checkout --username #{scm_username} --password #{scm_password} --no-auth-cache #{repository} #{release_path}"
      run "rm -f #{current_path} && ln -s #{latest_release} #{current_path}"
    end

  end
end


namespace :deploy do
  namespace :plx_engine do

    # Kickstart for deploying plx-engine
    task :default do
    end

  end
end

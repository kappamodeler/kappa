#
# Deploy recipe for plx_engines
#

namespace :release do
  namespace :plx_engine do

    # Kickstart for releasing plx-engine
    task :default do
      release.plx_engine.compile
    end
    # compilation
    task :compile, :roles => :job do
      run "cd /var/app/plx_engine/current; make;"
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

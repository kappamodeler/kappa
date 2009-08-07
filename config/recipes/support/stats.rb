namespace :stats do

  task :uptime do
    run "uptime"
  end


  task :disk_usage do
    run "df -h"
  end


end

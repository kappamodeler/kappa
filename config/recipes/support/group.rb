# Capistrano task that initializes our group_name
task :qa do
  set :group_name, 'qa'
  set :rails_env, 'qa'
  generic_roles
end

task :production do
  saywhat = Capistrano::CLI.ui.ask("Type 'production' to really do something to production!")
  raise "Didn't type 'production'" unless saywhat == 'production'
  set :group_name, 'production'
  set :rails_env, 'production'
  generic_roles
end

task :staging do
  set :group_name, 'staging'
  set :rails_env, 'staging' 
  generic_roles
end

task :release do
  set :group_name, 'release'
  set :rails_env, 'release'  
  generic_roles
end

task :custom do
  instances.split(/,/).each do |id|
    instance = Management::AWS::Instance.find(id)  
    role :generic, instance.dnsName if instance.dnsName
  end
end

task :set_group_name do
  set :group_name, group_name
  set :rails_env, group_name
  generic_roles
end

set :group_name do
  Capistrano::CLI.ui.ask("Enter a group/cluster to deploy to, leave blank for `default' :").downcase
end

# This is where we set our roles
task :generic_roles do
  @instances = Management::AWS::Instance.find_by_group_name(group_name)
  instance_count = @instances.size

  # We can optionallhy pass instances in from the command line, so we don't
  # operate on the entire group...
  #
  #   INSTANCE_ID=i-002,i-003 cap staging deploy
  instance_ids = ENV['INSTANCE_ID'].blank? ? [] : ENV['INSTANCE_ID'].split(',')
  if instance_ids.any?
    instance_count = instance_ids.size
    @instances.delete_if{|x| !instance_ids.include? x.instanceId }
  end

  if @instances.size != instance_count
    raise "\n!!\n!! Inconsitent count.  There are probably some instances you passed in INSTANCE_ID that don't appear in group #{group}"
  end

  @instances.each do |instance|
    puts "  * Setting `#{instance.dnsName}' to :generic role."
    role :generic, instance.dnsName if instance.dnsName
  end

  # So the rest of the cap tasks know what instances are
  # involved (and full AWS properties to them...)
  @@instances = @instances

end

task :reset_roles do
  # Use /etc/plectix/roles.yml or CURL the metadata
  command_string = <<-CMD
    perl -e "if (-e '/etc/plectix/roles.yml') { system 'cat /etc/plectix/roles.yml'; } else { system 'curl http://169.254.169.254/latest/user-data'; }"
  CMD

  channel_outputs = extras.real_capture command_string
  channel_outputs.each do |ch,data|
    server_roles = YAML::load(data)
    server_roles.each do |_role|
      puts "  * Setting `#{ch[:host]}' to `#{_role}'"
      if _role.to_sym == :db
        role _role.to_sym, ch[:host], :primary => true
      else
        role _role.to_sym, ch[:host]
      end
    end
  end
end



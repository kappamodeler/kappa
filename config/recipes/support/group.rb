# Capistrano task that initializes our group_name
task :qa do
  set :group_name, 'qa'
  generic_roles
end

task :production do
  saywhat = Capistrano::CLI.ui.ask("Type 'production' to really do something to production!")
  raise "Didn't type 'production'" unless saywhat == 'production'
  set :group_name, 'production'
  generic_roles
end

task :staging do
  set :group_name, 'staging'
  generic_roles
end

task :release do
  set :group_name, 'release'
  generic_roles
end

task :custom do
  instances.split(/,/).each do |id|
    instance = Management::AWS::Instance.find(id)  
    role :generic, instance.dnsName if instance.dnsName
  end
end

set :group_name do
  Capistrano::CLI.ui.ask("Enter a group/cluster to deploy to, leave blank for `default' :").downcase
end

# This is where we set our roles
task :generic_roles do
  @instances = Management::AWS::Instance.find_by_group_name(group_name)

  @instances.each do |instance|
    puts "  * Setting `#{instance.dnsName}' to :generic role."
    role :generic, instance.dnsName if instance.dnsName
  end
end

task :reset_roles do
  channel_outputs = extras.real_capture "cat /etc/plectix/roles.yml"
  channel_outputs.each do |ch,data|
    server_roles = YAML::load(data)
    server_roles.each do |_role|
      puts "  * Setting `#{ch[:host]}' to `#{_role}'"
      role _role.to_sym, ch[:host]
    end
  end
end



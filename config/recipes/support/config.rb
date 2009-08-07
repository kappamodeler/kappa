AWS_BUCKET_BY_ENV = {
  :production   => 'static.cellucidate.com',
  :staging      => 'static.staging.cellucidate.com',
  :integration  => 'static.plecticio.us',
  :qa           => 'static.qa.plecticio.us'
}

namespace :config do

  desc <<-DESC
  Setup config, including creating directories we know we need.  This 
  should be run after deploy:setup (i.e. doesn't need to run all the time
  DESC
  task :setup do
    [ '/etc/plectix', '/etc/monit.d/enabled' ].each do |dir|
      puts "  * Making #{dir} unless it exists..."
      run "/usr/bin/perl -e \"mkdir '#{dir}' unless -e '#{dir}'\""
    end
  end

  desc <<-DESC
  Records the AWS information locally for the instance
  DESC
  task :record_aws_keys do
    keyinfo = "aws_access_key_id: #{ENV['AWS_ACCESS_KEY_ID']}\n" +
              "aws_secret_access_key: #{ENV['AWS_SECRET_ACCESS_KEY']}"
    put keyinfo, "/etc/plectix/aws.yml"
  end


  desc <<-DESC
  Register the release versions with S3
  DESC
  task :register_release_version_with_s3 do
    _bootstrap_s3

    # There are two objects:
    #
    #   /plectix-deploy/registry/<group>/deployed/current
    #   /plectix-deploy/registry/<group>/deployed/all
    #
    # Current is a single entry, the release that is currently
    # deployed.  The archive is a single file that is an array
    # of release information (including the current release).
    release_bucket = "/plectix-deploy/registry/#{group_name}/deployed/"
    
    # 1. Delete the current release...
    AWS::S3::S3Object.delete('current', release_bucket)

    # 2. Build the current release entry and store
    registry_entry = {
      :revision => current_revision.gsub(/\n/,''),
      :created_at => Time.now,
      :group => group_name,
      :user => ENV['USER'],
      :hostname => `hostname`.gsub(/\n/,''),
      :branch => release_tag 
    }
    AWS::S3::S3Object.store('current', registry_entry.to_yaml, release_bucket)

    # 3. Put the current release in the top of 'all'
    all = YAML::load(AWS::S3::S3Object.value('all', release_bucket))
    all = [] unless all
    all << registry_entry
    AWS::S3::S3Object.delete('all', release_bucket)
    AWS::S3::S3Object.store('all', all.to_yaml, release_bucket)
  end

  desc <<-DESC
  Write shared_path/database.yml with database information for the cluster
  DESC
  task :database do
    get_primary_database

    unless exists?(:database_password)
      set :database_password, Digest::MD5.hexdigest("#{ENV['AWS_SECRET_ACCESS_KEY']}.#{group_name}")
      puts "  * Setting database password to #{database_password}"
    end 
    
    database_configuration = <<-EOF
#{group_name}:
  adapter: <%= (RUBY_PLATFORM == 'java')?'jdbcpostgresql':'postgresql' %>
  database: #{database_name}
  username: #{database_user}
  password: #{database_password}
  host: #{primary_database_private_hostname}
  pool: 10
EOF
    run "rm -f #{current_path}/config/database.yml"
    run "rm -f #{shared_path}/database.yml"
    put database_configuration, "#{shared_path}/database.yml"
    run "ln -s #{shared_path}/database.yml #{current_path}/config/database.yml"
  end

  desc <<-DESC
  Writes the amazon configuration to shared_path/amazon_s3.yml for the cluster"
  DESC
  task :amazon_s3 do
    bucket_name = "static.#{group_name}"
    bucket_name = AWS_BUCKET_BY_ENV[group_name.downcase.to_sym] if AWS_BUCKET_BY_ENV[group_name.downcase.to_sym]
    amazon_s3_configuration = <<-EOF
#{group_name}:
  bucket_name: #{bucket_name}
  access_key_id: #{ENV['AWS_ACCESS_KEY_ID']}
  secret_access_key: #{ENV['AWS_SECRET_ACCESS_KEY']}
EOF
    run "rm -f #{current_path}/config/amazon_s3.yml"
    run "rm -f #{shared_path}/amazon_s3.yml"
    put amazon_s3_configuration, "#{shared_path}/amazon_s3.yml"
    run "ln -s #{shared_path}/amazon_s3.yml #{current_path}/config/amazon_s3.yml"
  end


  task :_bootstrap_s3 do
    AWS::S3::Base.establish_connection!(:access_key_id => ENV['AWS_ACCESS_KEY_ID'],
                                        :secret_access_key => ENV['AWS_SECRET_ACCESS_KEY'])
  end    

  task :autoregister_database, :roles => :db, :only => { :primary => true } do
    _bootstrap_s3

    instance_id = capture("curl -s http://169.254.169.254/latest/meta-data/instance-id")
    AWS::S3::S3Object.store("primary_database_instance", instance_id, "/plectix-deploy/registry/#{group_name}")
    puts "  * Primary database is `#{instance_id}' for `#{group_name}'"
  end
  
  
  task :firewall do
    run "cd #{current_path} && RAILS_ENV=#{group_name} rake management:refresh_firewall"
  end

  task :rebuild do
    config.record_aws_keys
    config.update_apache_sites
  end

  task :update_apache_sites, :roles => :proxy do
    run "a2dissite default"
    run "a2ensite cellucidate"
    run "a2ensite cloud_control"
  end

  desc <<-DESC
  Resisters a primary database for a given cluster.
  DESC
  task :register_primary_database do
    _bootstrap_s3

    unless exists?(:instance_id)
      set :instance_id, Capistrano::CLI::ui.ask("Enter the instance_id for the primary database server : ")
    end
    AWS::S3::S3Object.store("primary_database_instance", instance_id, "/plectix-deploy/registry/#{group_name}")
    puts "  * Primary database is `#{instance_id}' for `#{group_name}'"
  end


  desc <<-DESC
  Get the primary database for the group
  DESC
  task :get_primary_database do
    _bootstrap_s3

    primary_database_instance_id = AWS::S3::S3Object.value("primary_database_instance", "/plectix-deploy/registry/#{group_name}/").gsub "\n", ""
    puts "  * Primary database instance is `#{primary_database_instance_id}'"
    set :primary_database_instance_id, primary_database_instance_id
    begin
      set :primary_database_private_hostname, Management::AWS::Instance.find(primary_database_instance_id).privateDnsName
    rescue NoMethodError
      raise "It doesn't look like instance #{primary_database_instance_id} is up!  Make sure the registry in S3 is up to date"
    end
    
    puts "  * Primary database is `#{primary_database_instance_id}' with " +
         "hostname `#{primary_database_private_hostname}' for `#{group_name}'"
  end

end


task :refresh_config do
  reset_roles

  cellucidate_ips = ''
  cloud_control_ips = ''
  api_ips = ''

  if @roles[:app]
    @roles[:app].servers.each do |server|
      cellucidate_ips += Management::AWS::Instance.find_by_dns_name(server.host).first.privateDnsName + ","
    end
  end

  if @roles[:management]
    @roles[:management].servers.each do |server|
      cloud_control_ips += Management::AWS::Instance.find_by_dns_name(server.host).first.privateDnsName + ","
    end
  end

  if @roles[:api]
    @roles[:api].servers.each do |server|
      api_ips += Management::AWS::Instance.find_by_dns_name(server.host).first.privateDnsName + ","
    end
  end

  puts " * Found cellucidate at #{cellucidate_ips}"
  puts " * Found cloud_control at #{cloud_control_ips}"

  run "cd #{current_path} && " + 
    "RAILS_ENV=#{group_name} " + 
    "CELLUCIDATE_IPS=#{cellucidate_ips} " +
    "CLOUD_CONTROL_IPS=#{cloud_control_ips} " +
    "API_IPS=#{api_ips} " +
    "rake server:refresh_config"
end

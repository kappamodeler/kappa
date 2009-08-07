require 'rake'
require 'net/http'
require 'uri'
require 'EC2'

# This is where our management tools are contained...
module Management

  module AWS
    @@ec2 = nil

    def AWS.ec2=(ec2)
      @@ec2 = ec2
    end

    def AWS.ec2
      return @@ec2 unless @@ec2.nil?
      @@ec2 = EC2::Base.new(:access_key_id => ENV['AWS_ACCESS_KEY_ID'], :secret_access_key => ENV['AWS_SECRET_ACCESS_KEY'])
    end

    module Instance
      
      # This is a lightweight implementation in this module for now.
      # This will move into a real active_record model in the clustered_servers
      # plugin later.
      def Instance.find(*args)
        describe_instances_response = Management::AWS.ec2.describe_instances
        all_instances = []
        describe_instances_response.reservationSet.item.each do |reservation|
          all_instances << reservation.instancesSet.item
        end
        all_instances.flatten!

        options = args.extract_options!

        case args.first
          when :first then all_instances.first
          when :last  then all_instances.last
          when :all   then find_every(options, describe_instances_response)
          else             find_from_ids(args, describe_instances_response)
        end
      end

      def Instance.find_by_group_name(group_name)
        find(:all, { :group_name => group_name })
      end

      def Instance.find_by_dns_name(dns_name)
        find(:all, { :dns_name => dns_name })
      end

      def Instance.find_all
        find(:all)
      end

      private

        def Instance.find_every(options, describe_instances_response)
          instances = []
          describe_instances_response.reservationSet.item.each do |reservation|
            groups = reservation.groupSet.item
            reservation.instancesSet.item.each do |instance|
              if (options.empty?)
                instances << instance
              end
              if (options[:group_name])
                instances << instance if groups.map(&:groupId).include?(options[:group_name])
              end
              if (options[:dns_name])
                instances << instance if instance.dnsName == options[:dns_name]
              end              
            end
          end
          instances
        end

        def Instance.find_from_ids(ids, describe_instances_response)
          instances = []
          describe_instances_response.reservationSet.item.each do |reservation|
            groups = reservation.groupSet.item
            reservation.instancesSet.item.each do |instance|
              if ids.kind_of?(String) or ids.kind_of?(Integer)
                return instance if instance.instanceId == ids
              elsif ids.kind_of?(Array) and ids.size == 1
                return instance if instance.instanceId == ids.first
              elsif ids.kind_of?(Array)
                instances << instance if (ids.include?(instance.instanceId))
              end
            end
          end
          instances
        end
      #:private
    end

    module Group

      def Group.find(*args)
        raise "Only :all is supported right now" unless args.first == :all
        raise "No additional args are supported now" if args[1]
        Management::AWS.ec2.describe_security_groups.securityGroupInfo.item
      end

      def Group.find_all
        find(:all)
      end

    end

  end

  module Role

    ROLES_FILE = '/etc/plectix/roles.yml'
    ROLES = [ 'app', 'management', 'job', 'javajob', 'db', 'proxy', 'search' ]

    def Role.current
      create_from_user_data! unless File.exists?(ROLES_FILE)
      YAML.load_file(ROLES_FILE)
    end

    def Role.set(*rolesin)
      File.open(ROLES_FILE, 'w') do |out|
        rolesin.flatten! if rolesin.kind_of?(Array)
        YAML.dump(rolesin, out)
      end
    end

    def Role.have_role?(role)
      return true if (role == 'any' or role == :any)
      current.flatten.include?(role)
    end

    # This will create /etc/plectix/plectix.yml from the string strored
    # in the AMI launch data user-data.  Since the user-data can't change
    # after the AMI is launched, we will touch a file to let us know the
    # information has already been written (/etc/plectix/.plectix.yml-created).
    #
    #   Management::Config.create_from_user_data!
    #
    # If a subsequent write is attempted, we raise an error. There is a force
    # option that will ignore the existance of the already-created file.
    #
    #   Management::Config.create_from_user_data!(true)
    #
    def Role.create_from_user_data!(force=false)
      if (File.exists?(ROLES_FILE) and !force)
        raise "User data has already been recorded!  Pass with force to overwrite."
      end

      # Make our plectix directory
      dir = ROLES_FILE.gsub /[^\/]*$/, ''
      Dir.mkdir(dir) unless File.directory?(dir)

      # Write out the information
      user_data = Management::RestfulParams.get 'user-data'
      f = File.new(ROLES_FILE, "w")
      f.write user_data
      f.close
    end

  end

  module RestfulParams

    def RestfulParams.get(uri_suffix, version='latest')
      uri_str = "http://169.254.169.254/#{version}/#{uri_suffix}"
      response = Net::HTTP.get_response(URI.parse(uri_str))
      case response
      when Net::HTTPSuccess then 
        response.body
      else
        nil
      end
    end

  end

  # Configuration management
  module Config

    # Take the items in ./management/conf/*** and evaluate them
    # as erb and then put them in the proper location.
    #
    # Management::Config.refresh!
    # Management::Config.refresh!(true)  # verbose output
    def Config.refresh!(verbose=true, prefix='/')
      Dir.glob("#{RAILS_ROOT}/lib/management/config/**/*.erb").each do |config|
        input = File.readlines(config).map {|l| l.rstrip}.join "\n"
        erb = ERB.new(input)
        outfile = config.gsub /\.erb$/, ''; outfile.gsub! /^.*\/config\//, prefix;
        File.open(outfile, 'w') do |f|
          puts " * attempting to update `#{outfile}" if verbose
          f.write(erb.result(binding))
          puts " * done! updated `#{outfile}'" if verbose
        end
      end
      sh "chmod +x /etc/init.d/*"  # Need execute bit...
    end
    
    def Config.current_groups
      Management::RestfulParams.get('meta-data/security-groups')
    end

  end

  # UFW (ubuntu firewall) settings
  # TODO: Move this into the management.rake task... there is no real need
  #       for it to be in here.
  module Firewall

    def Firewall.stop
      `/usr/sbin/ufw disable`
    end

    def Firewall.start
      `/usr/sbin/ufw enable`
    end

    def Firewall.restart
      Firewall.stop
      Firewall.start
    end

    # This is what the rules file looks like when there
    # are no rules.
    def Firewall.clean_slate
      <<-EOF
*filter
:ufw-user-input - [0:0]
:ufw-user-output - [0:0]
:ufw-user-forward - [0:0]
### RULES ###
-A ufw-user-input -j RETURN
-A ufw-user-output -j RETURN
-A ufw-user-forward -j RETURN
COMMIT
EOF
    end

    def Firewall.reset
      Firewall.stop

      File.open('/var/lib/ufw/user.rules', 'w') do |f|
        f.write(Firewall.clean_slate)
      end

      Firewall.start
    end
      

    def Firewall.update!
      Firewall.reset
      ufw_cmds = [ '/usr/sbin/ufw allow 22/tcp' ]
      Management::Role.current.each do |role|
        case role
        when 'proxy'
          ufw_cmds << '/usr/sbin/ufw allow 80/tcp'
          ufw_cmds << '/usr/sbin/ufw allow 443/tcp'
        when 'db'
          ufw_cmds << '/usr/sbin/ufw allow proto tcp from 10.0.0.0/24 to any port 5432'
        when 'app'
          ufw_cmds << '/usr/sbin/ufw allow proto tcp from 10.0.0.0/24 to any port 8000'
          ufw_cmds << '/usr/sbin/ufw allow proto tcp from 10.0.0.0/24 to any port 8001'
          ufw_cmds << '/usr/sbin/ufw allow proto tcp from 10.0.0.0/24 to any port 8002'
          ufw_cmds << '/usr/sbin/ufw allow proto tcp from 10.0.0.0/24 to any port 8003'
          ufw_cmds << '/usr/sbin/ufw allow proto tcp from 10.0.0.0/24 to any port 8004'
        when 'management'
          ufw_cmds << '/usr/sbin/ufw allow proto tcp from 10.0.0.0/24 to any port 9000'
          ufw_cmds << '/usr/sbin/ufw allow proto tcp from 10.0.0.0/24 to any port 9001'
        end
      end

      ufw_cmds.each do |cmd|
        puts " * running #{cmd} ..."
        puts `#{cmd}`
      end

    end

  end

  module Periodic

    def Periodic.run
      tasks_to_run = []

      nowish = Time.now
      current_minute = nowish.min
      current_hour = nowish.hour
      current_wday = nowish.wday
      current_mday = nowish.mday

      puts "current_minute = #{current_minute}"
      puts "current_hour = #{current_hour}"
      puts "current_wday = #{current_wday}"
      puts "current_mday = #{current_mday}"

      case current_minute.to_int
      when 0, 15, 30, 45, 59
        puts "adding"
        tasks_to_run << Rake::NameSpace[:quarter_hourly].tasks
      when 0
        tasks_to_run << Rake::Namespace[:hourly].tasks
        if current_hour.to_int == 0
          tasks_to_run << Rake::Namespace[:daily].tasks
        end
        if current_wday.to_int == 0
          tasks_to_run << Rake::Namespace[:weekly].tasks
        end
        if current_mday.to_int == 1
          tasks_to_run << Rake::Namespace[:monthly].tasks
        end
      end

      puts "Going to run #{tasks_to_run.inspect}"

      tasks_to_run.each do |task|
        #task.invoke
      end
      
    end
  end

  module Startup

    SERVICES = [ 'postgresql-8.3', 'apache2', 'ufw', 'monit', 
                 'cellucidate', 'cloud_control',
                 'job_server', 'small_job_server' ]

    def Startup.add(service)
      sh "chmod +x /etc/init.d/#{service}"
      sh "/usr/sbin/update-rc.d #{service} defaults"
    end

    def Startup.remove(service)
      sh "/usr/sbin/update-rc.d -f #{service} remove"
    end

    def Startup.remove_all!
      SERVICES.each do |service|
        sh "/etc/init.d/#{service} stop"
        Startup.remove(service)
      end
    end

  end

end

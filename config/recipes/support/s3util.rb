require File.expand_path(File.dirname(__FILE__) + '/../../initializers/patches')

namespace :s3 do

  desc <<-DESC
  Copies all objects in an S3 bucket to a target bucket. This will clobber the target before coping.
  DESC
  task :copy_bucket do
    config._bootstrap_s3
    
    set :s3_bucket_source, Capistrano::CLI::ui.ask("Enter the *SOURCE* bucket : ")
    set :s3_bucket_target, Capistrano::CLI::ui.ask("Enter the *DESTINATION* bucket : ")
    
    if s3_bucket_target == 'static.cellucidate.com' or s3_bucket_target == 's3.plectix.com'
    #if s3_bucket_target == 's3.plectix.com'
      raise "Can't overwrite production bucket!"
    end
    
    set :gotime, Capistrano::CLI::ui.ask("Continue? This will delete any objects in the target `#{s3_bucket_target}' (type 'y') : ")
    exit unless gotime =~ /y(es)?/i

    # XXX: Revisit the delete here
    #AWS::S3::Bucket.delete(s3_bucket_target, :force => true)
    if AWS::S3::Bucket.create(s3_bucket_target, :access => :public_read)
      target_bucket = AWS::S3::Bucket.find(s3_bucket_target)
    else
      raise "could not create bucket #{s3_bucket_target}"
    end
  
    source_bucket =  AWS::S3::Bucket.find(s3_bucket_source)
    all_objects = AWS::S3::Bucket.all_objects(s3_bucket_source)
    puts "found #{all_objects.size} objects to copy..."

    all_objects.each do |object|
      key = object.key
      next if key =~/\$folder\$$/
      # XXX: Keeping these two lines commented, it is to re-org s3.plectix.com to our newer structure
      # next unless key =~ /^production/
      # copy_key = key.gsub(/^production\//,'')
      puts " * Copying #{source_bucket.name} // #{key} to  #{target_bucket.name} // #{copy_key}"
      AWS::S3::S3Object.copy_to_target_bucket(object.key, copy_key, source_bucket.name, target_bucket.name, { :copy_acl => true })
    end

  end

end


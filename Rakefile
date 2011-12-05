# encoding: utf-8

require 'rubygems'
require 'bundler'
begin
  Bundler.setup(:default, :development)
rescue Bundler::BundlerError => e
  $stderr.puts e.message
  $stderr.puts "Run `bundle install` to install missing gems"
  exit e.status_code
end
require 'rake'

require 'jeweler'
Jeweler::Tasks.new do |gem|
  # gem is a Gem::Specification... see http://docs.rubygems.org/read/chapter/20 for more options
  gem.name = "unf_ext"
  gem.homepage = "http://github.com/knu/ruby-unf_ext"
  gem.license = "MIT"
  gem.summary = %Q{Unicode Normalization Form support library for CRuby}
  gem.description = %Q{Unicode Normalization Form support library for CRuby}
  gem.email = "knu@idaemons.org"
  gem.authors = ["Takeru Ohta", "Akinori MUSHA"]
  # dependencies defined in Gemfile
  gem.extensions << "ext/unf_ext/extconf.rb"
end
Jeweler::RubygemsDotOrgTasks.new

require 'rake/testtask'
Rake::TestTask.new(:test) do |test|
  test.libs << 'lib' << 'test'
  test.pattern = FileList['test/**/test_*.rb']
  test.verbose = true
end

require 'rcov/rcovtask'
Rcov::RcovTask.new do |test|
  test.libs << 'test'
  test.pattern = FileList['test/**/test_*.rb']
  test.verbose = true
  test.rcov_opts << '--exclude "gems/*"'
end

task :default => :test

require 'rake/rdoctask'
Rake::RDocTask.new do |rdoc|
  version = File.exist?('VERSION') ? File.read('VERSION') : ""

  rdoc.rdoc_dir = 'rdoc'
  rdoc.title = "unf_ext #{version}"
  rdoc.rdoc_files.include('README*')
  rdoc.rdoc_files.include('lib/**/*.rb')
end

require 'rake/extensiontask'
Rake::ExtensionTask.new('unf_ext') do |ext|
  ext.cross_compile = true
  ext.cross_platform = 'x86-mingw32'
end

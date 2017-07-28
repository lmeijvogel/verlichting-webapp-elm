Rake.application.options.trace_rules = true

require 'rake/clean'
require 'erb'

require './static_files_uploader.rb'

CLEAN.include(%w[tmp/**/* work/**/* dist/**/*])

desc "Build the development version"
task :default => :work

desc "Build the development version"
task :work => %w[work/index.html work/main.js]

desc "Build the production version"
task :dist => %w[set_production_vars fingerprinted_js dist/index.html]

task :set_production_vars do
  $env = "production"
end

# TODO: This clears the work directory as well
desc "Build the production version and deploy to the server."
task :deploy => %w[clean dist] do
  StaticFilesUploader.new('./deploy.yml').call
end

ELM_SOURCE_FILES = FileList['source/*']

ELM_TMP = "tmp/main.js"

directory "work/"
directory "dist/javascripts"
directory "tmp/"

file ELM_TMP => [ELM_SOURCE_FILES, "tmp/"].flatten do |task|
  config = $env == 'production' ? 'webpack.production.config.js' : 'webpack.config.js'

  puts "CONFIG: #{config}"
  `node_modules/.bin/elm-make --warn --output=#{task.name} source/Main.elm`
end

# For the fingerprinted files, no rule can be written since the
# filename is only known when it is run and a hash is calculated
task :fingerprinted_js => %w[fingerprinted_main_js]

task :fingerprinted_main_js => ["tmp/main.min.js", "dist/javascripts"] do |task|
  copy_fingerprinted_file(task.source, to: "dist/javascripts")
end

file "work/main.js" => [ELM_TMP, "work"] do |task|
  cp task.source, task.name
end

file "tmp/main.min.js" => ELM_TMP do |task|
  `node_modules/uglify-js/bin/uglifyjs #{task.source} --screw-ie8 --mangle --wrap --output #{task.name}`
end

file "work/index.html" => ["work", "source/index.html.erb"] do |task|
  write_index(output_path: "work", is_production: false)
end

file "dist/index.html" => ["source/index.html.erb"] do |task|
  write_index(output_path: "dist", is_production: true)
end

def write_index(output_path:, is_production:)
  puts "Writing index.html"
  File.open("source/index.html.erb", "r") do |erb|
    File.open("#{output_path}/index.html", "w") do |output|
      template = ERB.new(erb.read)

      output.write(template.result(binding))
    end
  end
end

def fingerprinted_file(path)
  source_fingerprint = Digest::SHA256.hexdigest(File.read(path))

  path.pathmap("%n-#{source_fingerprint}%x")
end

def copy_fingerprinted_file(source, to:)
  dest = File.join(to, fingerprinted_file(source))
  cp source, dest unless File.exists?(dest)
end

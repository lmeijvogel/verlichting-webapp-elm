require 'sshkit'
require 'sshkit/dsl'

include SSHKit::DSL
require 'yaml'
require 'pathname'

class StaticFilesUploader
  attr_reader :config_path

  def initialize(config_path)
    @config_path = config_path
  end

  def call
    Dir.chdir(config.local_dir)

    connection_string = sprintf("%s@%s", config.remote_user, config.remote_host)

    config = self.config

    on(connection_string) do
      base_path = Pathname.new(config.remote_base_dir)

      release_path = base_path.join(Time.new.strftime("%Y%m%d%H%M%S"))

      execute(:mkdir, "-p", release_path)
      Dir.glob("*").each do |file|
        upload!(file, release_path.join(file), recursive: true)
      end

      within(base_path) do
        execute(:ln, "-s", "-f", "-n", release_path, base_path.join("current"))

        # Lambda to have access to the SSHKit environment
        clean_old_releases = ->() {
          releases = capture(:find, %w|. -maxdepth 1 -regex '\.\/[0-9]+'|).split("\n").sort

          return if releases.length < 5

          old_release_paths = releases[0..releases.length-5]

          old_release_paths.each do |release_dir|
            path = base_path.join(release_dir)
            execute(:rm, "-rf", path)
          end
        }

        clean_old_releases.call
      end
    end
  end

  protected
  def config
    @config ||= MyConfig.new(config_path)
  end

  class MyConfig
    def initialize(path)
      @yaml = YAML.load(File.read(path))
    end

    def method_missing(method, *args)
      @yaml.fetch(method.to_s)
    end
  end
end

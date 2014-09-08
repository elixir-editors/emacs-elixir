desc "Create a new release."
task 'release' do
  current_version = run('git tag').split(/\n/).last.strip[1..-1]
  print "What version do you want to release? (current: #{current_version}): "
  version = STDIN.gets.strip
  version_tag = "v%s" % version

  if run('git tag').split(/\n/).include?(version_tag)
    raise("This tag has already been committed to the repo.")
  end

  run "./release.py v#{current_version} HEAD -t #{version_tag} -f CHANGELOG.md"

  elixir_mode_contents = File.read('elixir-mode.el')
  File.write('elixir-mode.el', update_version(elixir_mode_contents, current_version, version))
  git_changes(version, version_tag)
end

desc "Install latest Docker."
task 'docker:install:ubuntu:latest' do
  run_or_fail "[ -e /usr/lib/apt/methods/https ] || {
  apt-get update
  apt-get install apt-transport-https
}"
  run_or_fail "sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 \
--recv-keys 36A1D7869245C8950F966E92D8576A8BA88D21E9"
  run_or_fail "sudo sh -c \"echo deb https://get.docker.io/ubuntu docker main\
> /etc/apt/sources.list.d/docker.list\""
  run_or_fail "sudo apt-get update"
  run_or_fail "sudo apt-get install lxc-docker"
  docker_niceties()
end

desc "Install latest OS release of Docker."
task 'docker:install:ubuntu:stable' do
  run_or_fail "sudo apt-get update"
  run_or_fail "sudo apt-get install docker.io"
  docker_niceties "docker.io"
end

desc "Build Docker image."
task 'docker:build' do
  run_or_fail "sudo docker build --tag=emacs-elixir #{Dir.pwd}"
end

desc "Run tests on Docker image."
task 'docker:test' do
  pwd = Dir.pwd
  run_or_fail "sudo docker run -it -w #{pwd} -v #{pwd}:#{pwd} emacs-elixir:latest ./run_tests"
end

def docker_niceties(executable_name="docker")
  run_or_fail "sudo ln -sf /usr/bin/#{executable_name} /usr/local/bin/docker"
  run_or_fail "sudo sed -i '$acomplete -F _docker docker' \
/etc/bash_completion.d/#{executable_name}"
end

def git_changes(version, version_tag)
  run_or_fail "git commit -a -m \"prepare #{version}\""
  run_or_fail "git tag -a -m \"Version #{version}\" #{version_tag}"
  run_or_fail "git push origin"
  run_or_fail "git push origin --tags"
end

def update_version(content, from, to)
  content = content.gsub("Version: #{from}", "Version: #{to}")
  content = content.gsub("elixir-mode--version \"#{from}\"", "elixir-mode--version \"#{to}\"")
end

def run_or_fail(command)
  # Stolen shamelessly from
  # https://github.com/discourse/discourse/blob/a3863b52dbf6d6698ba92d063a1e0bc238014127/lib/tasks/docker.rake#L2-L6
  pid = Process.spawn(command)
  Process.wait(pid)
  $?.exitstatus == 0
end

def run(command)
  `#{command}`
end

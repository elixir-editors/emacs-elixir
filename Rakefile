desc "create a new release"
task 'release' do
  current_version = run('git tag').split(/\n/).last.strip[1..-1]
  print "What version do you want to release? (current: #{current_version}): "
  version = STDIN.gets.strip
  version_tag = "v%s" % version

  if run('git tag').split(/\n/).include?(version_tag)
    raise("This tag has already been committed to the repo.")
  end

  elixir_mode_contents = File.read('elixir-mode.el')
  File.write('elixir-mode.el', update_version(elixir_mode_contents, current_version, version))

  run "git commit -a -m \"prepare #{version}\""

  run "git tag -a -m \"Version #{version}\" #{version_tag}"
  run "git push origin"
  run "git push origin --tags"
end

def update_version(content, from, to)
  content = content.gsub("Version: #{from}", "Version: #{to}")
  content = content.gsub("elixir-mode--version \"#{from}\"", "elixir-mode--version \"#{to}\"")
end

def run(command)
  `#{command}`
end

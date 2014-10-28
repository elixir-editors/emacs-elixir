EMACS = "emacs"
CASK = "cask"

task default: %w[test]

desc "Create a new release."
task release: [:test] do
  current_version = run('git tag').split(/\n/).last.strip[1..-1]
  version = ask("What version do you want to release? (current: #{current_version}): ")
  version_tag = "v%s" % version

  if run('git tag').split(/\n/).include?(version_tag)
    raise("This tag has already been committed to the repo.")
  end

  run "./release.py v#{current_version} HEAD -t #{version_tag} -f CHANGELOG.md"

  elixir_mode_contents = File.read('elixir-mode.el')
  File.write('elixir-mode.el', update_version(elixir_mode_contents, current_version, version))
  git_changes(version, version_tag)
end

desc "Installed Emacs information"
task 'info' do
  process_info "Install Emacs information"
  say ""
  say "PATH: ", :green, false
  system "which #{EMACS}"
  say "VERSION: ", :green, false
  system "#{CASK} exec #{EMACS} --version | head -1"
end

desc "Run the test suite"
task "test" do
  process_info "Install package dependencies"
  say ""
  say "#{indent(3)}Command: ", :yellow, false
  sh "cask install"
  say ""

  process_info "Run test suite"
  say ""
  system "#{CASK} exec ert-runner"

  exit_when_failed!("Test suite failed!\n")
end

def git_changes(version, version_tag)
  run "git commit -a -m \"prepare #{version}\""
  run "git tag -a -m \"Version #{version}\" #{version_tag}"
  run "git push origin"
  run "git push origin --tags"
end

def update_version(content, from, to)
  content = content.gsub("Version: #{from}", "Version: #{to}")
end

def exit_when_failed!(message)
  if $?.exitstatus != 0
    warning(message)
    exit(1)
  end
end

def ansi
  {
    green:     "\e[32m",
    red:       "\e[31m",
    white:     "\e[37m",
    bold:      "\e[1m",
    on_green:  "\e[42m",
    ending:    "\e[0m"
  }
end

def run(command)
  `#{command}`
end

def say(message, type=:white, newline=true)
  message = "#{ansi[type]}#{message}#{ansi[:ending]}"
  if newline
    puts message
  else
    print message
  end
end

def process_info(message)
  puts "#{ansi[:on_green]}#{ansi[:red]}#{message}#{ansi[:ending]}#{ansi[:ending]}"
end

def info(message)
  puts "#{ansi[:green]}#{ansi[:bold]}#{message}#{ansi[:ending]}#{ansi[:ending]}"
end

def warning(message)
  puts "#{ansi[:red]}#{ansi[:bold]}#{message}#{ansi[:ending]}#{ansi[:ending]}"
end

def ask(question, type=:white)
  say(question, type, false)
  answer = STDIN.gets.chomp
  if answer == "" || answer.nil?
    return nil
  else
    return answer
  end
end

def indent(count)
  " " * count
end

def colorize(string, color)
  "#{ansi[color]}#{string}#{ansi[:ending]}"
end

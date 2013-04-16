
def cabal(command)
  sh "cabal #{command}" if command
end

def run_cabal(xs = [])
  xs.each {|t| cabal t}
end

task :default => [:build]

desc "Build the hgit binary"
task :build => [:clean] do
  commands = %w(configure build)
  run_cabal commands
end

desc "Build and copy the binary in the cabal bin directory"
task :copy => [:build] do
  run_cabal ["copy"]
end

desc "Run the tests"
task :test => [:clean] do
  commands = ["configure --enable-tests", "build" , "test --show-details=always"]
  run_cabal commands
end

desc "Clean artifacts"
task :clean do
  run_cabal ["clean"]
end

desc "Install required dependencies"
task :deps do
  run_cabal ["install --only-dependencies"]
end

namespace :dev do
  desc "Generate a ctags file"
  task :tags do
  `echo ":ctags" | ghci -v0 -isrc src/Main.hs`
  end
end

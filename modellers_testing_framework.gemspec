
Gem::Specification.new do |s|
  s.name = "modellers_testing_framework"
  s.platform = Gem::Platform::RUBY
  s.author = "Peter Lane"
  s.version = "1.0.1"
  s.email = "peter.lane@bcs.org.uk"
  s.summary = "A testing framework for developing computational modelling projects."
  s.license = "GPL3"
  s.description = <<-END
Testing framework for developing computational models.  The aim is to separate tests into 
groups, relating to their importance in the implemented scientific theory.
END
  s.files = [
    "COPYING.txt",
    "README.txt", 
    "lib/modellers_testing_framework.rb",
    "examples/example.rb",
  ]
  s.require_path = "lib"
  s.has_rdoc = true
  s.extra_rdoc_files << "README.txt"
end


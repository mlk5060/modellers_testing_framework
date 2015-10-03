repositories.remote << 'http://repo1.maven.org/maven2'

JUNIT = 'junit:junit:jar:4.11'

define 'mdk-test' do
  project.version = '1.0'
  compile.with JUNIT
  package :jar
end


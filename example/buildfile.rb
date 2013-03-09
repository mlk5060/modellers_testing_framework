repositories.remote << 'http://repo1.maven.org/maven2'

JUNIT = 'junit:junit:jar:4.11'
MTF = 'mdk-test:mdk-test:jar:1.0'

define 'example' do
  project.version = '1.0'
  compile.with JUNIT, MTF
end


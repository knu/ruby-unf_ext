require 'mkmf'

if with_config('static-libstdc++')
  $LDFLAGS << ' ' << `#{CONFIG['CC']} -print-file-name=libstdc++.a`.chomp
else
  have_library('stdc++')

  if RbConfig::CONFIG['host_os'] =~ /aix/
    # Compiler flags necessary on AIX.
    # rubocop:disable Style/GlobalVars
    $CFLAGS << ' ' << '-D_ALL_SOURCE=1'
    $CPPFLAGS << ' ' << '-D_ALL_SOURCE=1'
    $CXXFLAGS << ' ' << '-D_ALL_SOURCE=1'
  end
end

create_makefile 'unf_ext'

unless CONFIG['CXX']
  case CONFIG['CC']
  when %r{((?:.*[-/])?)gcc([-0-9.]*)$}
    cxx = $1 + 'g++' + $2
  when %r{((?:.*[-/])?)clang([-0-9.]*)$}
    cxx = $1 + 'clang++' + $2
  else
    cxx = CONFIG['CC']
  end

  warn "CXX is automatically set to #{cxx}"

  new_mf = <<-EOF << File.read('Makefile')
CXX=#{cxx}
  EOF

  File.open('Makefile', 'w') { |mf|
    mf.print new_mf
  }
end

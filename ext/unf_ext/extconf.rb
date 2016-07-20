require 'mkmf'

if with_config('static-libstdc++')
  $LDFLAGS << ' ' << `#{CONFIG['CC']} -print-file-name=libstdc++.a`.chomp
else
  have_library('stdc++')
end

case RUBY_PLATFORM
when /\Aarm/
  # A quick fix for char being unsigned by default on ARM
  if defined?($CXXFLAGS)
    $CXXFLAGS << ' -fsigned-char'
  else
    # Ruby < 2.0
    $CFLAGS << ' -fsigned-char'
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

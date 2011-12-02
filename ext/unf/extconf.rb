require 'mkmf'
have_library('stdc++')
have_header('ruby/encoding.h')
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

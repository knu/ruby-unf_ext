require 'mkmf'
$libs += " -lstdc++ "
have_header('ruby/encoding.h')
create_makefile 'unf'

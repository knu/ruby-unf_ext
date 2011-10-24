#!/usr/bin/env ruby

require 'pathname'

Dir.chdir(Pathname(__FILE__).dirname) {
  File.open('../data/normalization-test.txt', 'w:utf-8') { |t|
    ['NormalizationTest.txt', 'NormalizationTest.local.txt'].each { |file|
      File.open(file, 'r:utf-8') { |f|
        f.each_line { |line|
          next unless /^[0-9A-F]/ =~ line
          line.sub!(/ *#.*/, '')

          fields = line.split(';').map { |field|
            field.split.map { |hex| hex.to_i(16) }.pack('U*')
          }

          raise if fields.size != 6

          t.puts fields
        }
      }
    }
  }
}

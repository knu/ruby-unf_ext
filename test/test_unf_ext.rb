# -*- coding: utf-8 -*-
require 'helper'
require 'pathname'

class TestUnf < Test::Unit::TestCase
  should "raise ArgumentError if an unknown normalization form is given" do
    normalizer = UNF::Normalizer.new
    assert_raises(ArgumentError) { normalizer.normalize("が", :nfck) }
  end

  should "pass all tests bundled with the original unf" do
    normalizer = UNF::Normalizer.new
    open(Pathname(__FILE__).dirname + 'normalization-test.txt', 'r:utf-8').each_slice(6) { |lines|
      flunk "broken test file" if lines.size != 6 || lines.pop !~ /^$/
      str, nfd, nfc, nfkd, nfkc = lines
      assert nfd,  normalizer.normalize(str, :nfd)
      assert nfc,  normalizer.normalize(str, :nfd)
      assert nfkd, normalizer.normalize(str, :nfkd)
      assert nfkc, normalizer.normalize(str, :nfkc)
    }
  end
end

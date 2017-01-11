#! /usr/bin/ruby

load 'PascalTri.rb'
load 'LinCellAutomaton.rb'

#puts PascalTri.new(4)

puts LCA.new(ARGV[0].to_i || 3)

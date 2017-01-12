#! /usr/bin/ruby

load 'PascalTri.rb'
load 'LinCellAutomaton.rb'

size = ARGV.map(&:to_i)[0]
function = ARGV[1]
function = function.delete("_") if function

#puts PascalTri.new(size: size)

puts LCA.new(size: size, function: function)

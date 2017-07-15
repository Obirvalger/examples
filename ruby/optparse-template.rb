#!/usr/bin/env ruby
#/ Usage: <progname> [options]...
#/ How does this script make my life easier?
# ** Tip: use #/ lines to define the --help usage message.
$stderr.sync = true
require 'optparse'

# default options
flag    = false
option  = "default value"
integer = 23
list    = ["x", "y", "z"]

binding.pry

# parse arguments
file = __FILE__
ARGV.options do |opts|
  opts.on("-f", "--flag")              { flag = true }
  opts.on("-o", "--opt=val", String)   { |val| option = val }
  opts.on("-i", "--int=val", Integer)  { |val| integer = val }
  opts.on("--list=[x,y,z]", Array)     { |val| list = val }
  opts.on_tail("-h", "--help")         { exec "grep ^#/<'#{file}'|cut -c4-" }
  opts.parse!
end

# do your thing
warn "ARGV:     #{ARGV.inspect}"
warn "flag:     #{flag.inspect}"
warn "option:   #{option.inspect}"
warn "integer:  #{integer.inspect}"
warn "list:     #{list.join(',')}"

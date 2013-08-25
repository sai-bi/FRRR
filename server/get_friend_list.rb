#!/usr/bin/env ruby

# This program takes a valid Renren cookie (as string) from stdin and prints
# that user's friend list to stdout, one friend a line, e.g.,
#       "John Smith" "The University of Hong Kong" 12345678

# Currently, no error handling endeavor has been made.

require_relative 'renren'

cookie = gets
results = Renren.get_friend_list cookie
results.each { |r| puts "\"#{r[:name]}\" \"#{r[:school]}\" #{r[:id]}" }

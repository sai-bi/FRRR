#!/usr/bin/env ruby
require_relative 'renren'
require_relative 'r'

cookie = gets
renren = Renren::Renren.new cookie
r  = R::R.new

renren.get_friends [renren.me] do |friend|
  puts friend
  r.put_user friend
end

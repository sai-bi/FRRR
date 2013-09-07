#!/usr/bin/env ruby

require_relative 'renren'
require_relative 'r'

def batch_update_albums renren, r, users, count
  renren.get_albums(users) do |album|
    puts album
    r.put_album album
  end
  users = []; count = 0
end

cookie = gets
renren = Renren::Renren.new cookie
r  = R::R.new

users = []; count = 0
r.get_users do |user|
  users << user; count += 1
  batch_update_albums renren, r, users, count if count >= 200
end
batch_update_albums renren, r, users, count

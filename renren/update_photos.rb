#!/usr/bin/env ruby

require_relative 'renren'
require_relative 'r'

def batch_update_photos renren, r, albums, count
  renren.get_photos albums do |photo|
    puts photo
    r.put_photo photo
  end
  albums = []; count = 0
end

cookie = gets
renren = Renren::Renren.new cookie
r  = R::R.new

albums = []; count = 0
r.get_albums do |album|
  albums << album; count += 1
  batch_update_photos renren, r, albums, count if count >= 200
end
batch_update_photos renren, r, albums, count

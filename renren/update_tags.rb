#!/usr/bin/env ruby

require_relative 'renren'
require_relative 'r'

def batch_update_tags renren, r, photos, count
  renren.get_tags photos do |tag|
    puts tag
    r.put_tag tag
  end
  photos = []; count = 0
end

cookie = gets
renren = Renren::Renren.new cookie
r  = R::R.new

photos = []; count = 0
r.get_photos do |photo|
  photos << photo; count += 1
  batch_update_tags renren, r, photos, count if count >= 200
end
batch_update_tags renren, r, photos, count

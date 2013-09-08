#!/usr/bin/env ruby

require_relative 'renren'
require_relative 'r'

def batch_update_tags renren, r, photos, count
  renren.get_tags photos do |tag|
    puts tag
    r.put_tag tag
  end
end

cookie = gets
renren = Renren::Renren.new cookie
r  = R::R.new

photos = []; count = 0
r.get_photos do |photo|
  photos << photo; count += 1
  if count >= 200
    batch_update_tags renren, r, photos, count
    photos = []; count = 0
  end
end
batch_update_tags renren, r, photos, count

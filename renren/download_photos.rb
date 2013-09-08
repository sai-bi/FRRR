#!/usr/bin/env ruby

require_relative 'renren'
require_relative 'r'

def batch_download_photos renren, r, photos, count
  renren.get_photo_files photos do |photo, success, data|
    if success
      puts "[OK] #{photo[:user_id]} #{photo[:album_id]} #{photo[:photo_id]} #{photo[:caption]}"
      r.save_photo photo, data
      r.mark_photo_as_downloaded photo
    else
      puts "[Error] #{photo[:user_id]} #{photo[:album_id]} #{photo[:photo_id]} #{photo[:caption]}"
    end
  end
end

cookie = gets
renren = Renren::Renren.new cookie
r  = R::R.new

photos = []; count = 0
r.get_photos_not_download do |photo|
  photos << photo; count += 1
  if count >= 200
    batch_download_photos renren, r, photos, count
    photos = []; count = 0
  end
end
batch_download_photos renren, r, photos, count

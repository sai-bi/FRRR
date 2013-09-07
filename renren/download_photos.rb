#!/usr/bin/env ruby

require_relative 'renren'
require_relative 'r'

def batch_download_photos renren, r, batch, count
  renren.get_photo_files(batch) do |photo, success, file|
    if success
      puts "[OK] #{photo[:user_id]} #{photo[:album_id]} #{photo[:photo_id]} #{photo[:caption]}"
      r.save_photo photo, file
      r.mark_photo_as_downloaded photo
    else
      puts "[Error] #{photo[:user_id]} #{photo[:album_id]} #{photo[:photo_id]} #{photo[:caption]}"
    end
  end
  batch = []; count = 0
end

cookie = gets
renren = Renren::Renren.new cookie
r  = R::R.new

batch = []; count = 0
r.get_photos_not_download do |photo|
  batch << photo; count += 1
  batch_download_photos renren, r, batch, count if count >= 200
end
batch_download_photos renren, r, batch, count

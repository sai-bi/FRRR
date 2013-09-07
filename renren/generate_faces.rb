#!/usr/bin/env ruby

require 'RMagick'
require_relative 'r'

def generate_faces
  r = R::R.new
  r.get_tags do |tag|
    photo = { user_id:  tag[:owner_id],
              album_id: tag[:album_id],
              photo_id: tag[:photo_id] }
    next if !r.exists_photo photo
    puts tag

    begin
      image = Magick::ImageList.new (r.absolute_photo_path photo)
    rescue Magick::ImageMagickError
      next
    end

    cropped_image = image.crop tag[:x], tag[:y], tag[:width], tag[:height]
    r.save_tag tag, cropped_image
  end
end

generate_faces

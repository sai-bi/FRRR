#!/usr/bin/env ruby

require 'mysql2'
require 'fileutils'

module R

  # Centralized interface to database
  # This is one of the most vulnerable and yet critical part.
  # Therefore, design the interfaces to be as narrow and dedicated as possible.

  # Caution --------------------------------------------------------------
  #
  # A `false` of MySQL type BOOL has value 0 and it is casted into Ruby
  # value by mysql2 as is, but in Ruby only `nil` and `false` can be
  # counted as `false`!
  # http://stackoverflow.com/questions/3784042/why-cant-i-do-if-zero-in-ruby
  #
  # Had we not compared it explicitly against 1, a zero will be silently
  # deemed true in Ruby, which we actually meant false!
  #
  # Takeaway:
  # You should always explicitly compare a BOOl value from MySQL against 1.
  #
  # Exception:
  # :cast_booleans option in mysql2 is on

  class R
    def self.relative_photo_path photo
      user_id  = photo[:user_id]
      album_id = photo[:album_id]
      photo_id = photo[:photo_id]
      "photos/user-#{user_id}/album-#{album_id}/photo-#{photo_id}.jpg"
    end

    def self.relative_tag_path tag
      target_id = tag[:target_id]
      tag_id    = tag[:tag_id]
      "tags/target-#{target_id}/tag-#{tag_id}.jpg"
    end

    def initialize
      @conn = Mysql2::Client.new host:'localhost', username:'frrr'
      @conn.query 'use renren'
      @base_dir = '/media/Passport/renren'
    end

    def absolute_photo_path photo
      @base_dir + '/' + (self.class.relative_photo_path photo)
    end

    def get_users
      (@conn.query 'select * from users',
       cast_booleans: true).each(symbolize_keys: true) do |user|
        yield user
      end
    end

    def get_albums
      (@conn.query 'select * from albums',
       cast_booleans: true).each(symbolize_keys: true) do |album|
        yield album
      end
    end

    def get_photos
      (@conn.query 'select * from photos',
       cast_booleans: true).each(symbolize_keys: true) do |photo|
        yield photo
      end
    end

    def get_tags
      (@conn.query 'select * from tags',
       cast_booleans: true).each(symbolize_keys: true) do |tag|
        yield tag
      end
    end

    def count_users
      (@conn.query 'select count(*) from users').first['count(*)']
    end

    def count_albums
      (@conn.query 'select count(*) from albums').first['count(*)']
    end

    def count_photos
      (@conn.query 'select count(*) from photos').first['count(*)']
    end

    def count_tags
      (@conn.query 'select count(*) from tags').first['count(*)']
    end

    def get_photos_not_download
      (@conn.query \
       'select * from photos
        where downloaded = false',
        cast_booleans: true).each(symbolize_keys: true) do |photo|
          yield photo
        end
    end

    def count_photos_not_downloaded
      (@conn.query \
       'select count(*) from photos
        where downloaded = false').first['count(*)']
    end

    def mark_photo_as_downloaded photo
      user_id  = photo[:user_id]
      album_id = photo[:album_id]
      photo_id = photo[:photo_id]

      @conn.query \
        "update photos set downloaded = true
        where user_id = #{user_id}
        and album_id = #{album_id}
        and photo_id = #{photo_id}"
    end

    def exists_user user
      user_id  = user[:user_id]

      (@conn.query \
        "select exists (
          select * from users
          where user_id = #{user_id}
        )").first.values.first == 1
    end

    def exists_album album
      user_id  = album[:user_id]
      album_id = album[:album_id]

      (@conn.query \
        "select exists (
          select * from albums
          where user_id = #{user_id}
          and album_id = #{album_id}
        )").first.values.first == 1
    end

    def exists_photo photo
      user_id  = photo[:user_id]
      album_id = photo[:album_id]
      photo_id = photo[:photo_id]

      (@conn.query \
        "select exists (
          select * from photos
          where user_id = #{user_id}
          and album_id = #{album_id}
          and photo_id = #{photo_id}
        )").first.values.first == 1
    end

    def put_user user
      user_id = user[:user_id]
      name    = Mysql2::Client.escape user[:name]
      network = Mysql2::Client.escape user[:network]

      if !exists_user user
        @conn.query \
          "replace into
          users  (user_id, name, network)
          values (#{user_id}, '#{name}', '#{network}')"
      end
    end

    def put_album album
      user_id  = album[:user_id]
      album_id = album[:album_id]
      title    = Mysql2::Client.escape album[:title]
      private_ = album[:private]

      if !exists_album album
        @conn.query \
          "replace into
          albums (user_id, album_id, title, private)
          values (#{user_id}, #{album_id}, '#{title}', #{private_})"
      end
    end

    def put_photo photo
      user_id  = photo[:user_id]
      album_id = photo[:album_id]
      photo_id = photo[:photo_id]
      caption  = Mysql2::Client.escape photo[:caption]
      url      = Mysql2::Client.escape photo[:url]

      if !exists_photo photo
        @conn.query \
          "replace into
          photos (user_id, album_id, photo_id, caption, url)
          values (#{user_id}, #{album_id}, #{photo_id}, '#{caption}', '#{url}')"
      end
    end

    def put_tag tag
      owner_id  = tag[:owner_id]
      album_id  = tag[:album_id]
      photo_id  = tag[:photo_id]
      tag_id    = tag[:tag_id]
      target_id = tag[:target_id]
      x         = tag[:x]
      y         = tag[:y]
      width     = tag[:width]
      height    = tag[:height]

      @conn.query 'set foreign_key_checks=0'
      @conn.query \
        "replace into
        tags (owner_id, album_id, photo_id, tag_id, target_id, x, y, width, height)
        values (#{owner_id}, #{album_id}, #{photo_id}, #{tag_id}, #{target_id}, #{x}, #{y}, #{width}, #{height})"
      @conn.query 'set foreign_key_checks=1'
    end

    def save_photo photo, data
      FileUtils.cd @base_dir do
        photo_path = self.class.relative_photo_path photo
        FileUtils.mkdir_p (File.dirname photo_path)
        File.open(photo_path, 'w') { |f| f.write data }
      end
    end

    def save_tag tag, image
      FileUtils.cd @base_dir do
        tag_path = self.class.relative_tag_path tag
        FileUtils.mkdir_p (File.dirname tag_path)
        image.write tag_path
      end
    end
  end
end

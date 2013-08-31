#!/usr/bin/env ruby

# Wrappers around MySQL queries

require 'mysql2'

module Db

end

module Db::Renren

  module_function

  def get_photos conn
    conn.query 'use renren'
    conn.query 'select * from photos'
  end

  def count_photos conn
    conn.query 'use renren'
    (conn.query 'select count(*) from photos').first['count(*)']
  end

  def get_photos_downloaded conn
    conn.query 'use renren'
    conn.query 'select * from photos where downloaded = true'
  end

  def count_photos_downloaded conn
    conn.query 'use renren'
    (conn.query 'select count(*) from photos where downloaded = true').first['count(*)']
  end

  def get_photos_not_downloaded conn
    conn.query 'use renren'
    conn.query 'select * from photos where downloaded = false'
  end

  def count_photos_not_downloaded conn
    conn.query 'use renren'
    (conn.query 'select count(*) from photos where downloaded = false').first['count(*)']
  end

  def photo_has_been_downloaded conn, user_id, album_id, photo_id
    conn.query 'use renren'
    (conn.query \
      "select downloaded from photos
      where user_id  = #{user_id}
      and   album_id = #{album_id}
      and   photo_id = #{photo_id}").first['downloaded'] == 1
  end

  def mark_photo_as_downloaded conn, user_id, album_id, photo_id
    conn.query 'use renren'
    conn.query \
      "update photos set downloaded = true
      where user_id = #{user_id} and album_id = #{album_id} and photo_id = #{photo_id}"
  end

end

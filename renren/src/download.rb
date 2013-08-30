#!/usr/bin/env ruby

require 'fileutils'
require 'mysql2'
require 'mechanize'
require_relative 'renren'

def clear_print s
  print "\r\033[K"
  print s
end

module Download

  module_function

  def download conn, path
    clear_print "Downloading photos\n"

    conn.query 'use renren'
    agent = Mechanize.new

    clear_print '  Retrieving photos'

    photos_count = (conn.query 'select count(*) from photos').first['count(*)']
    success_count = 0
    failure_count = 0

    clear_print "  Retrieved #{photos_count} photos\n"
    photos_to_download = conn.query 'select * from photos where downloaded = false'
    photos_to_download_count = (conn.query 'select count(*) from photos where downloaded = false').first['count(*)']
    clear_print "  #{photos_to_download_count} to download\n"

    FileUtils.cd path do

      photos_to_download.each_with_index do |photo, i|
        clear_print "  #{i} of #{photos_to_download_count} (#{failure_count} failed) [Downloading] #{photo['url']}"
        download_success = false
        urls = Renren::make_backup_urls photo['url']
        FileUtils.mkdir_p Renren::make_album_path photo['user_id'], photo['album_id']
        urls.each do |url|
          begin
            agent.download url, (Renren::make_photo_path photo['user_id'], photo['album_id'], photo['photo_id'])
          rescue Mechanize::ResponseCodeError, Mechanize::ResponseReadError, Net::HTTP::Persistent::Error
            next
          else
            download_success = true
            conn.query \
              "update photos set downloaded = true
               where user_id = #{photo['user_id']} and album_id = #{photo['album_id']} and photo_id = #{photo['photo_id']}"
            break
          end
        end
        if download_success
          success_count += 1
        else
          failure_count += 1
        end
      end

    end

    clear_print "  #{success_count} photos downloaded, #{failure_count} failed\n"

  end
end

def test
  conn = Mysql2::Client.new host:'localhost', username:'frrr'
  Download::download conn, '/media/Passport/frrr/renren-photos'
end

test

#!/usr/bin/env ruby

require 'fileutils'
require 'mysql2'
require 'mechanize'
require_relative 'renren'
require_relative 'db'

def clear_print s
  print "\r\033[K"
  print s
end

module Download

  module_function

  def download conn, path

    clear_print "Downloading photos\n"

    agent = Mechanize.new

    photos_not_downloaded_count = Db::Renren::count_photos_not_downloaded conn
    success_count = 0
    skip_count = 0
    failure_count = 0

    clear_print '  Retrieving photos in local record'
    clear_print "  Retrieved #{Db::Renren::count_photos conn} photos in local record\n"
    clear_print "  #{photos_not_downloaded_count} to download\n"

    FileUtils.cd path do

      (Db::Renren::get_photos_not_downloaded conn).each_with_index do |photo, i|

        if (Db::Renren::photo_has_been_downloaded conn, photo['user_id'], photo['album_id'], photo['photo_id'])
          skip_count += 1
          next
        end

        clear_print \
          "  #{i} of #{photos_not_downloaded_count} "\
          "(#{skip_count} skipped, #{failure_count} failed) "\
          "[Downloading] #{photo['url']}"

        download_success = false
        FileUtils.mkdir_p (Renren::make_album_path photo['user_id'], photo['album_id'])

        (Renren::make_backup_urls photo['url']).each do |url|
          begin
            agent.download url, (Renren::make_photo_path photo['user_id'], photo['album_id'], photo['photo_id'])
          rescue Mechanize::ResponseCodeError, Mechanize::ResponseReadError, Net::HTTP::Persistent::Error
            next
          else
            download_success = true
            break
          end
        end

        if download_success
          success_count += 1
          Db::Renren::mark_photo_as_downloaded conn, photo['user_id'], photo['album_id'], photo['photo_id']
        else
          failure_count += 1
        end
      end

    end

    clear_print "  #{success_count} photos downloaded, #{skip_count} skipped, #{failure_count} failed\n"

  end
end

def test
  conn = Mysql2::Client.new host:'localhost', username:'frrr'
  Download::download conn, '/media/Passport/frrr/renren-photos'
end

test

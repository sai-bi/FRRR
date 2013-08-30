#!/usr/bin/env ruby

require 'fileutils'
require 'mysql2'
require 'mechanize'
require_relative 'renren'

module Download

  module_function

  def download conn, path
    puts "Downloading photos\n"

    conn.query 'use renren'
    agent = Mechanize.new

    puts '  Retrieving photos'

    photos_count = (conn.query 'select count(*) from photos').first['count(*)']
    success_count = 0
    failed_count = 0

    puts "  Retrieved #{photos_count} photos\n"
    photos_to_download = conn.query 'select * from photos where downloaded = false'
    photos_to_download_count = (conn.query 'select count(*) from photos where downloaded = false').first['count(*)']
    puts "  #{photos_to_download_count} to download"

    FileUtils.cd path do

      photos_to_download.each do |photo|
        #puts "  #{i} of #{photos_count} [Downloading photos]"
        #puts photo['caption']
        urls = Renren::make_backup_urls photo['url']
        urls.each do |url|
          begin
            #agent.download url, (make_photo_path user_id, album_id, photo_id)
          rescue Mechanize::ResponseCodeError, Mechanize::ResponseReadError, Net::HTTP::Persistent::Error, StandardError
            failed_count += 1
            next
          else
            success_count += 1
            url = conn.escape url
            #conn.query "update photos set downloaded = true where url = #{url}"
            break
          end
        end
      end

    end

    puts "  #{success_count} photos downloaded, #{failed_count} failed\n"

  end
end

def test
  conn = Mysql2::Client.new host:'localhost', username:'frrr'
  Download::download conn, '/media/Passport/frrr/renren-photos'
end

test

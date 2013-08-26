#!/usr/bin/env ruby

require "typhoeus"
require "fileutils"
require_relative "renren"

def main user_id
  FileUtils.cd "/media/Passport/frrr"

  cookie = gets
  renren = Renren::Login.new cookie

  FileUtils.mkdir "renren-photos" if !Dir.exists? "renren-photos"
  FileUtils.cd "renren-photos"

  (renren.get_friend_list user_id).shuffle.each do |friend|
    puts "friend #{friend}"

    user_id = friend[:user_id]

    FileUtils.mkdir "user-#{user_id}" if !Dir.exists? "user-#{user_id}"
    FileUtils.cd "user-#{user_id}"

    (renren.get_album_list user_id).shuffle.each do |album|
      puts "album #{album}"

      next if album[:private]

      album_id = album[:album_id]

      FileUtils.mkdir "album-#{album_id}" if !Dir.exists? "album-#{album_id}"
      FileUtils.cd "album-#{album_id}"


      (renren.get_an_album user_id, album_id).shuffle.each do |photo|
        puts "photo #{photo}"

        hydra = Typhoeus::Hydra.new

        request = Typhoeus::Request.new(
          photo[:new_url],
        )
        request.on_complete do |response|

          if response.code != 200
            $stderr.puts "\nNon-200 status code: #{response.code}\n\n#{response.headers}\n\n#{response.body}\n#{photo}\n\n\n"
            #exit 1
          elsif
            File.open "photo-#{photo[:photo_id]}.jpg", "w" do |file|
              file.write response.body
            end
          end
        end
        hydra.queue request

        hydra.run

      end
      puts ""
      FileUtils.cd ".."

    end
    FileUtils.cd ".."

  end
  FileUtils.cd ".."
end

#!/usr/bin/env ruby

require 'fileutils'
require 'mysql2'
require 'json'
require_relative 'renren'

def clear_print s
  print "\r\033[K"
  print s
end

module Update

  module_function

  def update_friends cookie, conn
    clear_print "Updating friends\n"

    conn.query 'use renren'

    user_id = (Renren::parse_cookie cookie)['id']

    clear_print "  Fetching friends of user #{user_id}"
    friends = Renren::list_my_friends cookie

    friends_count = friends.length
    clear_print "  Fetched #{friends_count} friends of user #{user_id}\n"

    friends.each_with_index do |friend, i|

      clear_print "  #{i} of #{friends_count} [Updating database] #{friend[:name]}"

      user_id = friend[:user_id]
      name    = friend[:name]
      network = friend[:network]

      conn.query \
        "replace into
         users  (user_id, name, network)
         values (#{user_id}, '#{conn.escape name}', '#{conn.escape network}')"

    end

    clear_print "  #{friends_count} friends found\n"

  end

  def update_albums cookie, conn
    clear_print "Updating albums\n"

    conn.query 'use renren'

    albums_count = 0

    clear_print '  Retrieving users'
    users = conn.query 'select * from users'
    users_count = (conn.query 'select count(*) from users').first['count(*)']
    clear_print "  Retrieved #{users_count} users\n"

    users.each_with_index do |user, i|

      clear_print "  #{i} of #{users_count} [Fetching albums] #{user['name']}"
      albums = Renren::list_albums cookie, user['user_id']

      next if albums.length == 0
      albums_count += albums.length

      clear_print "  #{i} of #{users_count} [Updating database] #{user['name']}"

      albums.each_with_index do |album, i|

        user_id  = user['user_id']
        album_id = album[:album_id]
        title    = conn.escape album[:title]
        private_ = album[:private] ? 'true' : 'false'

        conn.query \
          "replace into
           albums (user_id, album_id, title, private)
           values (#{user_id}, #{album_id}, '#{title}', #{private_})"
      end

    end

    clear_print "  #{albums_count} albums found\n"

  end

  def update_photos cookie, conn, path
    clear_print "Updating photos\n"

    FileUtils.cd path do
      conn.query 'use renren'

      photos_count = 0

      clear_print '  Retrieving albums'
      albums = conn.query 'select * from albums'
      albums_count = (conn.query 'select count(*) from albums').first['count(*)']
      failure_count = 0
      clear_print "  Retrieved #{albums_count} albums\n"

      albums.each_with_index do |album, i|

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

        next if album['private'] == 1

        clear_print "  #{i} of #{albums_count} (#{failure_count} failed) [Fetching photos] #{album['title']}"

        begin
          photos = Renren::list_photos cookie, album['user_id'], album['album_id']
        # list_photos parses a JSON page, which sometimes goes wrong.
        rescue JSON::ParserError
          failure_count += 1
          sleep 60
          next
        end

        photos_count += photos.length

        #clear_print "  #{i} of #{albums_count} (#{failure_count} failed) [Checking local files] #{album['title']}"
        album_path = Renren::make_album_path album['user_id'], album['album_id']
        local_file_names = (Dir.glob "#{album_path}/*").map do |file_name|
          File.basename file_name
        end

        clear_print "  #{i} of #{albums_count} (#{failure_count} failed) [Updating database] #{album['title']}"

        photos.each do |photo|

          expected_file_name = "photo-#{photo[:photo_id]}.jpg"

          user_id  = photo[:user_id]
          album_id = photo[:album_id]
          photo_id = photo[:photo_id]
          caption  = conn.escape photo[:caption]
          url      = conn.escape photo[:url]
          downloaded = (local_file_names.include? expected_file_name) ? 'true' : 'false'

          conn.query \
            "replace into
             photos (user_id, album_id, photo_id, caption, url, downloaded, cached_faces)
             values (#{user_id}, #{album_id}, #{photo_id}, '#{caption}', '#{url}', #{downloaded}, '')"

        end

      end

      clear_print "  #{photos_count} photos found. Failed to find photos for #{failure_count} albums.\n"

    end

  end

end

def test
  cookie = gets
  conn = Mysql2::Client.new host:'localhost', username:'frrr'
  Update::update_albums cookie, conn#, '/media/Passport/frrr/renren-photos'
end

test

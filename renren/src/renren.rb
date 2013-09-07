#!/usr/bin/env ruby

require 'typhoeus'
require 'nokogiri'
require 'uri'
require 'json'
require 'mysql2'
require 'fileutils'

module Renren

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

  class LocalClient
    def self.make_album_path album
      user_id  = album[:user_id]
      album_id = album[:album_id]
      "user-#{user_id}/album-#{album_id}"
    end

    def self.make_photo_path photo
      user_id  = photo[:user_id]
      album_id = photo[:album_id]
      photo_id = photo[:photo_id]
      "user-#{user_id}/album-#{album_id}/photo-#{photo_id}.jpg"
    end

    def initialize
      @conn = Mysql2::Client.new host:'localhost', username:'frrr'
      @conn.query 'use renren'
      @download_location = '/media/Passport/renren/photos'
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

    def put_user user
      user_id = user[:user_id]
      name    = Mysql2::Client.escape user[:name]
      network = Mysql2::Client.escape user[:network]

      @conn.query 'set foreign_key_checks=0'
      @conn.query \
        "replace into
        users  (user_id, name, network)
        values (#{user_id}, '#{name}', '#{network}')"
      @conn.query 'set foreign_key_checks=1'
    end

    def put_album album
      user_id  = album[:user_id]
      album_id = album[:album_id]
      title    = Mysql2::Client.escape album[:title]
      private_ = album[:private]

      @conn.query 'set foreign_key_checks=0'
      @conn.query \
        "replace into
        albums (user_id, album_id, title, private)
        values (#{user_id}, #{album_id}, '#{title}', #{private_})"
      @conn.query 'set foreign_key_checks=1'
    end

    def put_photo photo
      user_id    = photo[:user_id]
      album_id   = photo[:album_id]
      photo_id   = photo[:photo_id]
      caption    = Mysql2::Client.escape photo[:caption]
      url        = Mysql2::Client.escape photo[:url]

      @conn.query 'set foreign_key_checks=0'
      @conn.query \
        "replace into
        photos (user_id, album_id, photo_id, caption, url)
        values (#{user_id}, #{album_id}, #{photo_id}, '#{caption}', '#{url}')"
      @conn.query 'set foreign_key_checks=1'
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

    def save_photo photo, file
      FileUtils.cd @download_location do
        FileUtils.mkdir_p (self.class.make_album_path photo)
        File.open((self.class.make_photo_path photo), 'w') { |f| f.write file }
      end
    end
  end

  # Hand-made Renren API

  class RemoteClient
    def initialize cookie
      @cookie  = self.class.parse_cookie cookie
      @headers = self.class.make_headers cookie
      @user_id = @cookie['id']

      # Get requestToken and _rtk

      response = Typhoeus.get(
        "http://www.renren.com/#{@user_id}",
        headers: @headers
      )
      if !response.success?
        STDERR.puts 'Renren: failed to initialize an instance'
        exit 1
      end
      doc = Nokogiri::HTML response.body
      script = (doc.css 'script')[0].text
      xn = (script.split ";\n")[0]
      values = xn.scan /'[\d\w]*'/
      @request_token = values[0].delete "'"
      @rtk           = values[1].delete "'"
    end

    # Parse a cookie (as string) into a Ruby hash

    def self.parse_cookie cookie
      (cookie.split '; ').inject({}) do |acc, record|
        a = record.split '='
        acc[a[0]] = a[1]
        acc
      end
    end

    def self.make_headers cookie
      { 'Connection'      => 'keep-alive',
        'Accept'          => 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
        'User-Agent'      => 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/29.0.1547.57 Safari/537.36',
        'DNT'             => '1',
        'Accept-Language' => 'en-US,en;q=0.8',
        'Cookie'          => cookie
      }
    end

    def self.replace_url_host url
      interchangeable_hosts = ['fmn.rrimg.com', 'fmn.rrfmn.com', 'fmn.xnpic.com']
      uri = URI.parse url
      if interchangeable_hosts.include? uri.host
        uri.host = 'fmn.rrimg.com'
        uri.to_s
      else
        url
      end
    end

    public

    def me
      { user_id: @user_id }
    end

    def get_friends users
      hydra = Typhoeus::Hydra.new
      users.each do |user|
        user_id = user[:user_id]

        # Determine last_page, i.e., the number of pages of friend list
        # Note that the index starts from 0.

        request = Typhoeus::Request.new(
          'http://friend.renren.com/GetFriendList.do',
          params: { 'curpage' => 0, 'id' => user_id },
          headers: @headers
        )
        request.on_complete do |response|
          doc = Nokogiri::HTML response.body
          begin
            last_page = ((doc.css 'div#topPage.page a')[-1]['href'].scan /\d+/)[0].to_i
          rescue NoMethodError

            # This occurs when the user has only one page of friends, e.g.:
            # http://friend.renren.com/GetFriendList.do?id=334665088

            last_page = 0
          end

          hydra = Typhoeus::Hydra.new
          (0..last_page).each do |page|
            request = Typhoeus::Request.new(
              'http://friend.renren.com/GetFriendList.do',
              params: { 'curpage' => page, 'id' => user_id },
              headers: @headers
            )
            request.on_complete do |response|
              if response.success?
                doc = Nokogiri::HTML response.body
                friends = doc.css 'ol#friendListCon li div.info dl'
                friends.each do |friend|
                  friend =
                    { user_id: friend.css('dd a')[0]['href'].scan(/\d+/)[0].to_i,
                      name:    friend.css('dd')[0].text.strip,
                      network: friend.css('dd')[1].text.strip
                    }
                  yield friend
                end
              end
            end
            hydra.queue request
          end
          hydra.run
        end
        hydra.queue request
      end
      hydra.run
    end

    def get_albums users
      hydra = Typhoeus::Hydra.new
      users.each do |user|
        user_id = user[:user_id]
        request = Typhoeus::Request.new(
          "http://photo.renren.com/photo/#{user_id}/album/relatives/ajax",
          params: { 'offset' => 0, 'limit' => 2**31-1 },
            headers: @headers
        )
        request.on_complete do |response|
          if response.success?
            doc = Nokogiri::HTML response.body
            albums = doc.css 'li a.album-title'
            albums.each do |album|
              album =
                { user_id:  user_id,
                  album_id: (album['href'].scan /\d+/)[1].to_i,
                  title:    (album.css 'span.album-name').text.strip,
                  private:  !(album.css 'span.album-name i.privacy-icon.picon-password').empty?
                }
              yield album
            end
          end
        end
        hydra.queue request
      end
      hydra.run
    end

    def get_photos albums
      hydra = Typhoeus::Hydra.new
      albums.each do |album|
        next if album[:private]
        user_id  = album[:user_id]
        album_id = album[:album_id]
        request = Typhoeus::Request.new(
          "http://photo.renren.com/photo/#{user_id}/album-#{album_id}/bypage/ajax",
          params: { 'curPage' => 0, 'pagenum' => 2**31-1 },
            headers: @headers
        )
        request.on_complete do |response|
          if response.success?
            begin
              json = JSON.parse response.body
            rescue JSON::ParserError
              next
            end

            photos = json['photoList']
            photos.each do |photo|
              if photo['largeUrl'] == ''
                next

                # Actually, this case has happened and caused our program to crash, e.g.:
                #
                # http://photo.renren.com/photo/266765826/album-434542658/bypage/ajax?curPage=0&pagenum=20
                # http://photo.renren.com/photo/266765826/album-434542658
                #
                # In this case, we just ignore such missing photo.

              end
              photo =
                { user_id:  user_id,
                  album_id: album_id,
                  photo_id: photo['photoId'].to_i,
                  caption:  photo['title'].strip,
                  #time:     photo['time'].strip,
                  url:      photo['largeUrl'].strip
                }
              yield photo
            end
          end
        end
        hydra.queue request
      end
      hydra.run
    end

    def get_photo_files photos
      hydra = Typhoeus::Hydra.new
      photos.each do |photo|
        request = Typhoeus::Request.new(
          (self.class.replace_url_host photo[:url]),
          #headers: @headers,
          timeout: 30
        )
        request.on_complete do |response|
          if response.success?
            success = true
            file = response.body
          else
            success = false
            file = nil
          end
          yield photo, success, file
        end
        hydra.queue request
      end
      hydra.run
    end

    # Although album_id is not needed to get photo tags, we nevertheless include
    # it to make a uniform interface with other functions in this module as well
    # as the schema of the `tags` table in our database.

    def get_tags photos
      hydra = Typhoeus::Hydra.new
      photos.each do |photo|
        user_id  = photo[:user_id]
        album_id = photo[:album_id]
        photo_id = photo[:photo_id]
        request = Typhoeus::Request.new(
          "http://photo.renren.com/photo/#{user_id}/photo-#{photo_id}/ajax",
          method: :post,
            headers: @headers,
            body: { 'psource' => '0',
                    'requestToken' => @request_token,
                    '_rtk' => @rtk
                  }
        )
        request.on_complete do |response|
          if response.success?
            begin
              json = JSON.parse response.body
            rescue JSON::ParserError
              next
            end

            tags = json['tags']
            next if tags.nil?

            # We deliberately avoid the usage of `user_id` since it is ambiguous: whether
            # it refers to the owner of the photo or the person being tagged?

            tags.each do |tag|
              tag =
                { owner_id: user_id,
                  album_id: album_id,
                  photo_id: photo_id,
                  tag_id: (tag['id'].to_s.scan /\d+/)[0].to_i,
                  target_id: tag['targetId'],
                  x: tag['leftToPhoto'],
                  y: tag['topToPhoto'],
                  width:  tag['frameWidth'],
                  height: tag['frameHeight']
                }

              # target_id == 0 iff the tag does not link to a user

              next if tag[:target_id] == 0

              yield tag
            end
          end
        end
        hydra.queue request
      end
      hydra.run
    end
  end
end

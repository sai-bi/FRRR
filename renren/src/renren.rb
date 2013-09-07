#!/usr/bin/env ruby

require 'typhoeus'
require 'nokogiri'
require 'uri'
require 'json'

module Renren

  # Hand-made Renren API

  class Renren
    def initialize cookie
      @cookie  = self.class.parse_cookie cookie
      @headers = self.class.make_headers cookie
      @user_id = @cookie['id'].to_i

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
      values = xn.scan /'[-\d\w]*'/
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

#!/usr/bin/env ruby

require 'mechanize'
require 'typhoeus'
require 'nokogiri'
require 'json'
require 'uri'

module Renren
  # All functions below are exposed
  module_function

  # Parse a cookie (as string) into a Ruby hash
  def parse_cookie cookie
    (cookie.split '; ').inject({}) do |acc, item|
      a = item.split '='
      acc[a[0]] = a[1]
      acc
    end
  end

  def make_headers cookie
    { 'Connection'      => 'keep-alive',
      'Accept'          => 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
      'User-Agent'      => 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/29.0.1547.57 Safari/537.36',
      'DNT'             => '1',
      'Accept-Language' => 'en-US,en;q=0.8',
      'Cookie'          => cookie
    }
  end

  def list_friends cookie, user_id
    # Determine last_page, i.e., the number of pages of friend list
    response = Typhoeus.get(
      'http://friend.renren.com/GetFriendList.do',
      params: { curpage: 0, id: user_id },
      headers: (make_headers cookie)
    )
    doc = Nokogiri::HTML response.body
    last_page = ((doc.css 'div#topPage.page a')[-1]['href'].scan /\d+/)[0].to_i

    # Make parallel requests using Hydra
    hydra = Typhoeus::Hydra.new

    # Define requests and queue them in Hydra. Won't actually fire them up.
    requests = (0..last_page).map do |page|
      Typhoeus::Request.new(
        'http://friend.renren.com/GetFriendList.do',
        params: { curpage: page, id: user_id },
        headers: (make_headers cookie)
      )
    end

    requests.each { |request| hydra.queue request }

    # Fire up Hydra!
    hydra.run

    # Handle responses
    (requests.map do |request|

      doc = Nokogiri::HTML request.response.body
      friends = doc.css 'ol#friendListCon li div.info dl'

      friends.inject([]) do |acc, friend|
        name    = friend.css('dd')[0].text.strip
        user_id = friend.css('dd a')[0]['href'].scan(/\d+/)[0].to_i
        network = friend.css('dd')[1].text
        acc << { name: name, network: network, user_id: user_id}
      end

    end).flatten 1
  end

  def list_my_friends cookie
    list_friends cookie, (parse_cookie cookie)['id']
  end

  def list_albums cookie, user_id
    response = Typhoeus.get(
      "http://photo.renren.com/photo/#{user_id}/album/relatives",
      headers: (make_headers cookie)
    )

    doc = Nokogiri::HTML response.body
    (doc.css 'div.album-list.othter-album-list a.album-title').map do |album|
      { title:    (album.css 'span.album-name').text.strip,
        user_id:  user_id,
        album_id: (album['href'].scan /\d+/)[1].to_i,
        # A <span> tag of class 'password' differentiates a private album
        # from a public one. The latter does not have one.
        private: !(album.css 'span.album-name span.password').empty?
      }
    end
  end

  # Takes an original image url and returns an array of urls (with the original
  # one included of course), which should be attempted in the same order
  def make_backup_urls url
    interchangeable_hosts = ['fmn.rrimg.com', 'fmn.rrfmn.com', 'fmn.xnpic.com']
    uri = URI url
    if interchangeable_hosts.include? uri.host
      interchangeable_hosts.map do |host|
        uri.host = host
        uri.to_s
      end
    else
      [url]
    end
  end

  # The desired album must not be private.
  def list_photos cookie, user_id, album_id
    response = Typhoeus.get(
      "http://photo.renren.com/photo/#{user_id}/album-#{album_id}/bypage/ajax?curPage=0&pagenum=#{2**31-1}",
      headers: (make_headers cookie)
    )

    (JSON.parse response.body)['photoList'].inject([]) do |acc, photo|
      acc <<
      { photo_id: photo['photoId'].to_i,
        user_id:  user_id,
        album_id: album_id,
        caption:  photo['title'],
        time:     photo['time'],
        urls:     (make_backup_urls photo['largeUrl'])
      }
    end
  end

  def make_album_path user_id, album_id
    "user-#{user_id}/album-#{album_id}"
  end

  def make_photo_path user_id, album_id, photo_id
    "#{make_album_path user_id, album_id}/photo-#{photo_id}.jpg"
  end

  # The desired album must not be private.
  def download_album cookie, user_id, album_id
    album_path = make_album_path user_id, album_id
    FileUtils.mkdir_p album_path

    agent = Mechanize.new

    photos = list_photos cookie, user_id, album_id
    photos.each do |photo|
      photo_id = photo[:photo_id]

      photo[:urls].each do |url|
        retry_count = 0

        begin
          agent.download url, (make_photo_path user_id, album_id, photo_id)

        rescue Mechanize::ResponseCodeError, Mechanize::ResponseReadError, Net::HTTP::Persistent::Error
          if retry_count < 1
            retry_count += 1
            agent.shutdown
            sleep 10
            agent = Mechanize.new
            retry
          else
            next
          end

        # Raise all exceptions within StandardError
        rescue
          $stderr.puts photo
          raise

        # Proceed to next photo if nothing bad happens
        else
          break
        end
      end
    end
  end
end

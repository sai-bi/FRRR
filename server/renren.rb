#!/usr/bin/env ruby

require "typhoeus"
require "nokogiri"
require "json"
require "uri"

module Renren

  # Parse a cookie (as string) into a Ruby hash
  def self.parse_cookie cookie
    (cookie.split "; ").inject({}) do |acc, item|
      a = item.split "="
      acc[a[0]] = a[1]
      acc
    end
  end

  class Login
    include Renren
    attr_reader :headers

    def initialize cookie
      @cookie = cookie
      @id     = (Renren::parse_cookie cookie)["id"].to_i
      @headers =
        { "Connection"      => "keep-alive",
          "Accept"          => "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          "User-Agent"      => "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/29.0.1547.57 Safari/537.36",
          "DNT"             => "1",
          "Accept-Language" => "en-US,en;q=0.8",
          "Cookie"          => cookie
        }
    end

    def get_friend_list user_id

      # Determine last_page, i.e., the number of pages of friend list
      response = Typhoeus.get(
          "http://friend.renren.com/GetFriendList.do",
          params: { curpage: 0, id: user_id },
          headers: @headers,
        )
      doc = Nokogiri::HTML response.body
      last_page = ((doc.css "div#topPage.page a")[-1]["href"].scan /\d+/)[0].to_i

      # Make parallel requests using Hydra
      hydra = Typhoeus::Hydra.new

      # Define requests and queue them in Hydra. Won't actually fire them up.
      requests = (0..last_page).map do |page|
        Typhoeus::Request.new(
          "http://friend.renren.com/GetFriendList.do",
          params: { curpage: page, id: user_id },
          headers: @headers
        )
      end

      requests.each { |request| hydra.queue request }

      # Fire up Hydra!
      hydra.run

      # Handle responses
      (requests.map do |request|

        doc = Nokogiri::HTML request.response.body
        friends = doc.css "ol#friendListCon li div.info dl"

        friends.inject([]) do |acc, friend|
          name    = friend.css("dd")[0].text.strip
          user_id = friend.css("dd a")[0]["href"].scan(/\d+/)[0].to_i
          network = friend.css("dd")[1].text
          acc << { name: name, network: network, user_id: user_id}
        end

      end).flatten 1

    end

    def get_album_list user_id
      response = Typhoeus.get(
        "http://photo.renren.com/photo/#{user_id}/album/relatives",
        headers: @headers,
      )

      doc = Nokogiri::HTML response.body
      (doc.css "div.album-list.othter-album-list a.album-title").map do |album|
        { title:    (album.css "span.album-name").text.strip,
          user_id:  user_id,
          album_id: (album["href"].scan /\d+/)[1].to_i,
          # A <span> tag of class "password" differentiates a private album
          # from a public one. The latter does not have one.
          private: ! (album.css "span.album-name span.password").empty?
        }
      end
    end

    def get_an_album user_id, album_id
      response = Typhoeus.get(
        "http://photo.renren.com/photo/#{user_id}/album-#{album_id}/bypage/ajax?curPage=0&pagenum=#{2**31-1}",
        headers: @headers
      )

      (JSON.parse response.body)["photoList"].inject([]) do |acc, photo|

        old_url = photo["largeUrl"]

        # Replace slow host "fmn.xnpic.com"
        uri = URI old_url
        if uri.host == "fmn.xnpic.com"
          uri.host = "fmn.rrimg.com"
        end
        new_url = uri.to_s

        acc << { photo_id: photo["photoId"].to_i,
          caption: photo["title"],
          time:    photo["time"],
          old_url: old_url,
          new_url: new_url
        }

      end
    end

  end # End of class "Renren"

end # End of module "Renren"

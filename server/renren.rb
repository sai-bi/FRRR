#!/usr/bin/env ruby

require 'typhoeus'
require 'nokogiri'

module GetFriendList

  # Just define a request. Won't actually fire it up.
  def GetFriendList.define_request page, id, cookie
    defiine_request = Typhoeus::Request.new(
      'http://friend.renren.com/GetFriendList.do',
      params: { curpage: page, id: id },
      headers:
      { 'Connection'      => 'keep-alive',
        'Accept'          => 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
        'User-Agent'      => 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/29.0.1547.57 Safari/537.36',
        'DNT'             => '1',
        'Accept-Language' => 'en-US,en;q=0.8',
        'Cookie'          => cookie
      }
    )
  end

  def GetFriendList.handle_response response
    doc = Nokogiri::HTML response.response_body
    friends = doc.css 'ol#friendListCon li div.info dl'

    friends.inject([]) do |acc, f|
      name   = f.css('dd')[0].text.strip
      id     = f.css('dd a')[0]['href'].scan(/\d+/)[0].to_i
      school = f.css('dd')[1].text
      acc << { name: name, school: school, id: id}
    end
  end

  def GetFriendList.get_friend_list cookie
    id = (Renren::parse_cookie cookie)['id'].to_i

    # Determine last_page, i.e., the number of pages of friend list
    request = define_request 0, id, cookie
    request.run
    doc = Nokogiri::HTML(request.response.response_body)
    last_page = ((doc.css 'div#topPage.page a')[-1]['href'].scan /\d+/)[0].to_i

    # Make parallel requests using Hydra
    hydra = Typhoeus::Hydra.new

    # Define our requests and queue them in Hydra
    requests = (0..last_page).map { |page| define_request page, id, cookie }
    requests.each { |request| hydra.queue request }

    # Fire up Hydra!
    hydra.run

    # Handle responses
    (requests.map { |r| handle_response r.response }).flatten 1
  end

end

module Renren

  # Parse a cookie (as string) into a Ruby hash
  def Renren.parse_cookie cookie
    (cookie.split '; ').inject({}) do |acc, item|
      a = item.split '='
      acc[a[0]] = a[1]
      acc
    end
  end

  include GetFriendList
  def Renren.get_friend_list cookie
    GetFriendList::get_friend_list cookie
  end

end

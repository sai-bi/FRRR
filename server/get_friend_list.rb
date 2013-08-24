#!/usr/bin/env ruby

# Usage:
#       ./get_friend_list.rb COOKIE

# This program takes a valid Renren cookie (as string) and prints that user's
# friend list to standard output, one friend a line,
# e.g.,
#       "John Smith" "The University of Hong Kong" 12345678

# Currently, no error handling endeavor has been made.

require 'typhoeus'
require 'nokogiri'

# Parse a Renren cookie (as string) into a Ruby hash
def parse_cookie cookie
  (cookie.split '; ').inject({}) do |acc, item|
    a = item.split '='
    acc[a[0]] = a[1]
    acc
  end
end

# Just define a request. Won't actually fire it up.
def define_request page, id, cookie
  defiine_request = Typhoeus::Request.new(
    'http://friend.renren.com/GetFriendList.do',
    params: { curpage: page, id: id },
    headers: {
      #'Connection'      => 'keep-alive',
      #'Accept'          => 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
      #'User-Agent'      => 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/29.0.1547.57 Safari/537.36',
      #'DNT'             => '1',
      #'Accept-Language' => 'en-US,en;q=0.8',
      'Cookie'           => cookie
    }
  )
end

def main cookie
  id = (parse_cookie cookie)['id'].to_i

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

  # Parse response bodies
  requests.each do |request|
    doc = Nokogiri::HTML request.response.response_body
    friends = doc.css 'ol#friendListCon li div.info dl'

    friends.each do |friend|
      name   = friend.css('dd')[0].text.strip
      id     = friend.css('dd a')[0]['href'].scan(/\d+/)[0]
      school = friend.css('dd')[1].text
      puts "\"#{name}\" \"#{school}\" #{id}"
    end
  end
end

main ARGV[0]

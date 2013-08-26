#!/usr/bin/ruby -w

while s = gets
  if s =~ /https?:\/\/.+\..+/i
    system "google-chrome --incognito #{s.strip} &"
  end
end

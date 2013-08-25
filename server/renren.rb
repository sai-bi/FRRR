module Renren
  # Parse a cookie (as string) into a Ruby hash
  def Renren.parse_cookie cookie
    (cookie.split '; ').inject({}) do |acc, item|
      a = item.split '='
      acc[a[0]] = a[1]
      acc
    end
  end
end

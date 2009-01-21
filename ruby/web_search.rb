#!/usr/bin/env ruby
# 
#  web_search.rb
#  
#  Created by Lars Yencken on 2009-01-21.
#  Copyright 2009 Lars Yencken. All rights reserved.
# 
#  From: http://developer.yahoo.com/ruby/ruby-xml.html
#

require 'net/http';
require 'rexml/document';

# Web search for "cheese"
url = 'http://api.search.yahoo.com/WebSearchService/V1/webSearch?appid=YahooDemo&query=cheese&results=2'

# Get the XML data as a string
xml_data = Net::HTTP.get_response(URI.parse(url)).body

# Extract event information
doc = REXML::Document.new(xml_data)
titles = []
links = []

doc.elements.each('ResultSet/Result/Title') do |ele|
  titles << ele.text
end

doc.elements.each('ResultSet/Result/Url') do |ele|
  links << ele.text
end

# Print all events
titles.each_with_index do |title, index|
  print "#{title} => #{links[index]}\n"
end
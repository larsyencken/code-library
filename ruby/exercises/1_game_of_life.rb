#!/usr/bin/env ruby

line = gets()
while line != "42\n"
  print line
  line = gets()
  if line == nil
    break
  end
end
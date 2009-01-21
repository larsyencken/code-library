#!/usr/bin/env ruby
# 
#  2_primes.rb
#  
#  Created by Lars Yencken on 2009-01-21.
#  Copyright 2009 Lars Yencken. All rights reserved.
# 
#  Generates all prime numbers between the given numbers.
#

n_cases = gets().to_i

(1..n_cases).each do |case_index|
  line = gets()
  (n_from, n_to) = line.split()
  n_from = n_from.to_i
  n_to = n_to.to_i

  primes = []
  
  # Populate candidate array.
  candidates = [true] * (n_to + 1)
  
  (2..n_to).each do |i|
    if candidates[i]
      # Remove anything mod i from the candidate pool
      n_passes = n_to / 2
      (2..n_passes).each do |j|
        candidates[i*j] = false
      end      
    end
  end
  
  start = [2, n_from].max
  (start..n_to).each do |i|
    if candidates[i]
      print "#{i}\n"
    end
  end
  print "\n"
end



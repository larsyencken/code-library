#!/usr/bin/env julia
#
#  uniq.jl
#
#  Like the unix uniq command, don't output sequential duplicate lines.
#

if length(ARGS) != 1
    println("Usage: uniq.jl <filename>")
    exit(1)
end

filename = ARGS[1]
istream = open(filename)

last = None
for l in each_line(istream)
    if l != last
        print(l)
    end
    last = l
end

close(istream)

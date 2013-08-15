#!/usr/bin/env julia
#
#  uniq.jl
#
#  Like the unix uniq command, don't output sequential duplicate lines.
#

if length(ARGS) == 0
    istream = STDIN
elseif length(ARGS) == 1
    istream = open(ARGS[1])
else
    println("Usage: uniq.jl <filename>")
    exit(1)
end

last = None
for l in eachline(istream)
    if l != last
        print(l)
    end
    last = l
end

close(istream)

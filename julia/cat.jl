#!/usr/bin/env julia
#
#  cat.jl
#

if length(ARGS) > 0
    for filename in ARGS
        f = open(filename)
        for l in eachline(f)
            write(STDOUT, l)
        end
        close(f)
    end
else
    for l in eachline(STDIN)
        write(STDOUT, l)
    end
end

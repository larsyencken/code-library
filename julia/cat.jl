#!/usr/bin/env julia
#
#  cat.jl
#

if length(ARGS) > 0
    for filename in ARGS
        f = open(filename)
        for l in each_line(f)
            write(STDOUT, l)
        end
        close(f)
    end
else
    for l in each_line(STDIN)
        write(STDOUT, l)
    end
end

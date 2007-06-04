#
# Makefile
#

CFLAGS = -I/sw/include/python2.4 -L/sw/lib -lpython2.4

default: primes.dylib

clean:
	rm -f *.c *.o *.dylib

%.o: %.c
	gcc -c -dynamic $(CFLAGS) $<

%.dylib: %.o
	gcc -bundle -dylib -o `basename $< .o`.dylib $(CFLAGS) $<

%.c: %.pyx
	pyrexc $<


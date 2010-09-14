#
# Makefile
#

CC = 6g
LD = 6l

EXE = sieve

all: $(EXE)

sieve: sieve.6
	$(LD) -o $(EXE) sieve.6
	

clean:
	rm -f *.6

clobber: clean
	rm -rf $(EXE)

%.6: %.go
	$(CC) $<

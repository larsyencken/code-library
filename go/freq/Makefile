#
# Makefile
#

CC = 6g
LD = 6l

EXE = example

all: $(EXE)

example: freq.6 example.6
	$(LD) -o $(EXE) example.6
	

clean:
	rm -f *.6

clobber: clean
	rm -rf $(EXE)

%.6: %.go
	$(CC) $<

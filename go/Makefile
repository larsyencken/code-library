#
# Makefile
#

CC = 6g
LD = 6l

EXE = hello echo

all: $(EXE)

clean:
	rm -f *.6

clobber: clean
	rm -rf $(EXE)

%: %.6
	$(LD) -o $@ $<

%.6: %.go
	$(CC) $<

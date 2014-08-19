#
#  Makefile
#

TARGETS = cat-string cat-bytestring

default: $(TARGETS)

cat-string: cat-string.hs
	ghc -O2 -o $@ $<

cat-bytestring: cat-bytestring.hs
	ghc -O2 -o $@ $<

bench: $(TARGETS)
	@echo 'Benching cat-string'
	pv -Ss 1G /dev/zero | ./cat-string >/dev/null
	@echo
	@echo 'Benching cat-bytestring'
	pv -Ss 1G /dev/zero | ./cat-bytestring >/dev/null
	@echo

clean:
	rm -f $(TARGETS) *.o *.hi

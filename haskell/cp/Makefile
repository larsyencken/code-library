TARGETS = cp-strict cp-lazy

default: $(TARGETS)

cp-strict: cp-strict.hs
	ghc -O2 -o $@ $<

cp-lazy: cp-lazy.hs
	ghc -O2 -o $@ $<

clean:
	rm -f $(TARGETS) *.{hi,o}

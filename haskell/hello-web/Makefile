TARGETS = hello-web

.PHONY: default clean serve

default: $(TARGETS)

hello-web: hello-web.hs
	ghc -O2 -o $@ $<

serve: hello-web
	@echo 'Listening on http://localhost:8000/'
	@./hello-web

clean:
	rm -f $(TARGETS) *.{hi,o}

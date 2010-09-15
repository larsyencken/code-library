#
#  Makefile
#

all: tokenize

tokenize: lines.6 tokenize.6 
	6l -o tokenize tokenize.6

clean:
	rm -f tokenize.6 lines.6

clobber: clean
	rm -f tokenize

%.6: %.go
	6g $<

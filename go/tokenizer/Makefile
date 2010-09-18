#
#  Makefile
#

all: tokenize tokenize_vector tokenize_fast

tokenize_vector: lines.6 tokenize_vector.6
	6l -o tokenize_vector tokenize_vector.6

tokenize_fast: tokenize_fast.6
	6l -o tokenize_fast tokenize_fast.6

tokenize: lines.6 tokenize.6 
	6l -o tokenize tokenize.6

clean:
	rm -f tokenize.6 lines.6

clobber: clean
	rm -f tokenize

%.6: %.go
	6g $<

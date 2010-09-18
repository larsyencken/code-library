tokenizer: tokenizer.o
	g++ -O3 -o tokenizer tokenizer.o

%.o: %.cpp
	g++ -c $<

clean:
	rm -f tokenizer.o

clobber: clean
	rm -f tokenizer

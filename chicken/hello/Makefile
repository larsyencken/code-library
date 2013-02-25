
CFLAGS = -I/usr/local/include/chicken
LDFLAGS = -lchicken -lc -lcrt1.o

default: hello

hello.c: hello.scm
	chicken $<

hello.o: hello.c
	$(CC) -c $(CFLAGS) $<

hello: hello.o
	$(LD) -o $@ $(LDFLAGS) $<

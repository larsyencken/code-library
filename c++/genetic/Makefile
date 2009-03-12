#
# Makefile
#

CC = g++
OBJS =  point.o move.o board.o evaluate.o genetic.o trainer.o
CFLAGS = -g

TARGET = trainer

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS)

%.o: %.cpp
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o

clobber: clean
	rm -f $(TARGET)

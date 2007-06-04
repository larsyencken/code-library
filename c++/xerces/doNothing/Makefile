#
# Makefile
#

CC = g++
TARGET = doNothing
SRC = $(TARGET).cpp
CXXFLAGS = -Wall
LIBS = -lxerces-c

default = $(TARGET)

$(TARGET): $(SRC)
	$(CC) -o $(TARGET) $(CXXFLAGS) $(SRC) $(LIBS)

clean:
	rm -f $(TARGET)

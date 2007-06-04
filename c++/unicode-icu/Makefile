#
# Makefile
#

CC = g++
TARGET = testUnicode
SRC = testUnicode.cpp
LIBS = -I/usr/include/unicode

default = testUnicode

$(TARGET): $(SRC)
	$(CC) -o $(TARGET) $(SRC) $(LIBS)

clean:
	rm -f $(TARGET)

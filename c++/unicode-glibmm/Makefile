#
# Makefile
#

CC = g++
TARGET = testUnicode
SRC = testUnicode.cpp
LIBS = `pkg-config glibmm-2.4 --cflags --libs`

default = testUnicode

$(TARGET): $(SRC)
	$(CC) -o $(TARGET) $(SRC) $(LIBS)

clean:
	rm -f $(TARGET)

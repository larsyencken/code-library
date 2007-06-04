#
# Makefile
#

CC = g++
TARGET = stats
INCLUDE = -I/usr/include/python2.4
LIBS = -lboost_python -lpython2.4
CXXFLAGS = -shared `pkg-config glibmm-2.4 --cflags --libs`

default: $(TARGET).so

%.so: %.cpp
	$(CC) -o `basename $< .cpp`.so $(CXXFLAGS) $(INCLUDE) $(LIBS) $<

clean:
	rm $(TARGET).so

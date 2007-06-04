#
# Makefile
#

CC = g++
TARGETS = cppString.so pyString.so
LIBS = -lpython2.4 -lboost_python
INC = -I/usr/include/python2.4
CXXFLAGS = -shared -O2

default: $(TARGETS)

%.so: %.cpp
	$(CC) -o `basename $< .cpp`.so $(CXXFLAGS) $(INC) $(LIBS) $<

clean:
	rm -f $(TARGETS)

#
# Makefile
#

CC = g++
TARGETS = cppString.so pyString.so
LIBS = -lpython2.6 -lboost_python
INC = -I/usr/include/python2.6
CXXFLAGS = -shared -O2 -fpic

default: $(TARGETS)

%.so: %.cpp
	$(CC) -o `basename $< .cpp`.so $(CXXFLAGS) $(INC) $(LIBS) $<

clean:
	rm -f $(TARGETS)

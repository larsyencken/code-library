#
# Makefile
#

CC = g++
INCLUDE = -I/usr/include/python2.4
LIBS = -lboost_python -lpython2.4 `pkg-config glibmm-2.4 --libs`
CXXFLAGS = -O2 -shared `pkg-config glibmm-2.4 --cflags`
TARGETS = kana_cpp.so

default: $(TARGETS)

%.so: %.cpp
	$(CC) -o $@ -shared $(CXXFLAGS) $(INCLUDE) $(LIBS) $<

clean:
	rm -f *.pyc *.pyo

clobber: clean
	rm -f $(TARGET)

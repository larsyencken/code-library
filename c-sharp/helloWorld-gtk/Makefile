#
# Makefile
#

CC = mcs
PACKAGES = -pkg:gtk-sharp-2.0
LIBS = -r:System.Drawing

default: helloworld.exe

%.exe: %.cs
	$(CC) $(PACKAGES) $(LIBS) $<

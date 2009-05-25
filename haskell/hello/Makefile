
TARGET = hello

$(TARGET): $(TARGET).hs
	ghc -o $(TARGET) $(TARGET).hs

clean:
	rm -f $(TARGET).hi $(TARGET).o

clobber: clean
	rm -f $(TARGET)

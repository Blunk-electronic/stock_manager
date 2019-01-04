TARGET = stock_manager

VPATH = lib
OBJS = $(TARGET).o sm_string_processing.o sm_csv.o

# Edit here if you want to install the binary by default in home/your_home_dir/bin :
#PREFIX = $(HOME)
PREFIX = /usr/local

BINDIR = $(PREFIX)/bin

# compile
.adb.o:
	gcc -c $< -I $(VPATH)
.SUFFIXES: .adb .o

# link
$(TARGET): $(OBJS)
	gnatbind -x $(TARGET).ali; gnatlink $(TARGET).ali

install:
#	install -D -s $(TARGET) $(BINDIR)/$(TARGET) 
# if debug symbols are required:
	install -D $(TARGET) $(BINDIR)/$(TARGET)

uninstall:
	@rm -f $(BINDIR)/$(TARGET)

clean:
	@rm -f *.o *.ali $(TARGET)
	



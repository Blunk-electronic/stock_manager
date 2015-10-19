VPATH = include
OBJS = stock_manager.o csv.o m1.o m1_internal.o 

# compile
.adb.o:
	gcc -c $< -I include
.SUFFIXES: .adb .o

# link
stock_manager: $(OBJS)
	gnatbind -x stock_manager.ali; gnatlink stock_manager.ali

clean:
	rm *.o *.ali



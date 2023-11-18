CFLAGS = $(XC_CFLAGS) -I../cfitsio

OBJS = votParse.o votParse_f77.o votParse_spp.o votExpatCB.o	\
    votElement.o votAttr.o votStack.o votHandle.o votUtil_spp.o

INCS = votParse.h votParse_spp.h

%.o: %.x
	$(XC) -c $? -o $@

libVOTable.a: $(OBJS) 
	ar r $@ $?

install: libVOTable.a
	mkdir -p $(bindir) $(includedir)
	cp libVOTable.a $(bindir)
	cp $(INCS) $(includedir)

clean:
	rm -f $(OBJS) libVOTable.a

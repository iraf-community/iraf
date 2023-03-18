OBJS := y1.o y2.o y3.o y4.o

hlib: xyacc.e
	cp $? $(hlib)

xyacc.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

clean:
	rm -f $(OBJS) xyacc.e

.PHONY: hlib clean

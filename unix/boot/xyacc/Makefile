OBJS := y1.o y2.o y3.o y4.o

hbin: xyacc.e
	cp $? $(hbin)

xyacc.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

clean:
	rm -f $(OBJS) xyacc.e

.PHONY: hlib clean

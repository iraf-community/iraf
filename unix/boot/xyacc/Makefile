OBJS := y1.o y2.o y3.o y4.o

xyacc.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

install: xyacc.e
	cp $? $(bindir)

clean:
	rm -f $(OBJS) xyacc.e

.PHONY: install clean

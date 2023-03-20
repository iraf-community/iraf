OBJS := rmfiles.o

rmfiles.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

install: rmfiles.e
	cp $? $(bindir)

clean:
	rm -f $(OBJS) rmfiles.e

.PHONY: install clean

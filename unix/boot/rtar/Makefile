OBJS := rtar.o

rtar.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

install: rtar.e
	cp $? $(bindir)

clean:
	rm -f $(OBJS) rtar.e

.PHONY: install clean

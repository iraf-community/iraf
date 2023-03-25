OBJS := wtar.o

wtar.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

install: wtar.e
	cp $? $(bindir)

clean:
	rm -f $(OBJS) wtar.e

.PHONY: install clean

OBJS := main.o char.o fdcache.o fncache.o host.o pkg.o scanlib.o sflist.o tok.o

mkpkg.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

install: mkpkg.e
	cp $? $(bindir)

clean:
	rm -f $(OBJS) mkpkg.e

.PHONY: install clean

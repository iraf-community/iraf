OBJS := main.o char.o fdcache.o fncache.o host.o pkg.o scanlib.o	\
        sflist.o tok.o

hlib: mkpkg.e
	cp $? $(hlib)

mkpkg.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

clean:
	rm -f $(OBJS) mkpkg.e

.PHONY: hlib

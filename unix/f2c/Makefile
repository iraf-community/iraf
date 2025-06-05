install: src/f2c libf2c/libf2c.a
	mkdir -p $(includedir) $(bindir)
	cp libf2c/libf2c.a $(bindir)
	cp src/f2c $(bindir)f2c.e
	cp libf2c/f2c.h $(includedir)

clean:
	$(MAKE) -C src -f makefile.u $@
	$(MAKE) -C libf2c -f makefile.u $@

src/f2c:
	$(MAKE) -C src -f makefile.u f2c

libf2c/libf2c.a:
	$(MAKE) -C libf2c -f makefile.u f2c.h libf2c.a

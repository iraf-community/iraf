hbin: src/f2c libf2c/libf2c.a
	cp libf2c/libf2c.a $(hbin)
	cp src/f2c $(hbin)f2c.e

clean:
	$(MAKE) -C src -f makefile.u $@
	$(MAKE) -C libf2c -f makefile.u $@

src/f2c:
	$(MAKE) -C src -f makefile.u f2c

libf2c/libf2c.a:
	$(MAKE) -C libf2c -f makefile.u libf2c.a

OBJS := rpp.o rppfor/librpp.a ratlibf/libf.a ratlibc/libc.a

rpp.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(FLIBS) -o $@

install: rpp.e
	cp $? $(bindir)

rppfor/librpp.a: rppfor
	$(MAKE) -C rppfor librpp.a

ratlibf/libf.a: ratlibf
	$(MAKE) -C ratlibf libf.a

ratlibc/libc.a:
	$(MAKE) -C ratlibc libc.a

clean:
	rm -f $(OBJS) rpp.e
	$(MAKE) -C rppfor $@
	$(MAKE) -C ratlibf $@
	$(MAKE) -C ratlibc $@

# If the RATFOR environment variable is defined (and points to the
# ratfor preprocessor), regenerate the Fortran files from the Ratfor
# sources.

rppfor:
	if [ "$(RATFOR)" ] ; then \
	  rm -f rppfor/*.f rpprat/fort ; \
	  $(MAKE) -C rpprat RATFOR=${RATFOR} ; \
	fi

ratlibf:
	if [ "$(RATFOR)" ] ; then \
	  rm -f ratlibf/*.f ratlibr/fort ; \
	  $(MAKE) -C ratlibr RATFOR=${RATFOR} ; \
	fi

.PHONY: install clean rppfor ratlibf

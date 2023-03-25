OBJS := xc.o

xc.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

install: xc.e
	cp $? $(bindir)
	$(MAKE) -C xpp $@
	$(MAKE) -C rpp $@

clean:
	rm -f $(OBJS) xc.e
	$(MAKE) -C xpp $@
	$(MAKE) -C rpp $@

.PHONY: install clean

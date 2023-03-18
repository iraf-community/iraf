OBJS := xc.o

hbin: xc.e
	cp $? $(hbin)
	$(MAKE) -C xpp $@
	$(MAKE) -C rpp $@

xc.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

clean:
	rm -f $(OBJS) xc.e
	$(MAKE) -C xpp $@
	$(MAKE) -C rpp $@

.PHONY: hlib clean

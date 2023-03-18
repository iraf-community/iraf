OBJS := xppmain.o lexyy.o xppcode.o decl.o

hlib: xpp.e
	cp $? $(hlib)

xpp.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

lexyy.c: xpp.l

clean:
	rm -f $(OBJS) xpp.e

.PHONY: hlib clean

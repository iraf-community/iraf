OBJS := xppmain.o lexyy.o xppcode.o decl.o

xpp.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

install: xpp.e
	cp $? $(bindir)

lexyy.c: xpp.l
	flex -o $@ -l $^

clean:
	rm -f $(OBJS) xpp.e

.PHONY: install clean

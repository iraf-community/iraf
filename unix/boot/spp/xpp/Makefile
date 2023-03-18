OBJS := xppmain.o lexyy.o xppcode.o decl.o

hbin: xpp.e
	cp $? $(hbin)

xpp.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

lexyy.c: xpp.l
	flex -o $@ -l $^

clean:
	rm -f $(OBJS) xpp.e

.PHONY: hlib clean

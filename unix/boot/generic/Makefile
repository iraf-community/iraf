OBJS := generic.o lexyy.o yywrap.o

hlib: generic.e
	cp $? $(hlib)

lexyy.c: tok.l
	flex -o $@ $^

generic.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

clean:
	rm -f $(OBJS) lexyy.c generic.e

.PHONY: hlib clean

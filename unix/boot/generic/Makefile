OBJS := generic.o lexyy.o yywrap.o

generic.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

install: generic.e
	cp $? $(bindir)

lexyy.c: tok.l
	flex -o $@ $^

clean:
	rm -f $(OBJS) lexyy.c generic.e

.PHONY: install clean

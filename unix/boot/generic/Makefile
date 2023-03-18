OBJS := generic.o lexyy.o yywrap.o

hbin: generic.e
	cp $? $(hbin)

lexyy.c: tok.l
	flex -o $@ $^

generic.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

clean:
	rm -f $(OBJS) lexyy.c generic.e

.PHONY: hlib clean

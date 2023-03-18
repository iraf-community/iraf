OBJS := rtar.o

hlib: rtar.e
	cp $? $(hlib)

rtar.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

clean:
	rm -f $(OBJS) rtar.e

.PHONY: hlib clean

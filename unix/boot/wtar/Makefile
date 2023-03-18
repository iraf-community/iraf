OBJS := wtar.o

hlib: wtar.e
	cp $? $(hlib)

wtar.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

clean:
	rm -f $(OBJS) wtar.e

.PHONY: hlib clean

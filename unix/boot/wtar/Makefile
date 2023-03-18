OBJS := wtar.o

hbin: wtar.e
	cp $? $(hbin)

wtar.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

clean:
	rm -f $(OBJS) wtar.e

.PHONY: hlib clean

OBJS := rtar.o

hbin: rtar.e
	cp $? $(hbin)

rtar.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

clean:
	rm -f $(OBJS) rtar.e

.PHONY: hlib clean

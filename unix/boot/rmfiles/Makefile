OBJS := rmfiles.o

hlib: rmfiles.e
	cp $? $(hlib)

rmfiles.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

clean:
	rm -f $(OBJS) rmfiles.e

.PHONY: hlib clean

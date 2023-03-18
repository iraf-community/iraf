OBJS := rmbin.o

hlib: rmbin.e
	cp $? $(hlib)

rmbin.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

clean:
	rm -f $(OBJS) rmbin.e

.PHONY: hlib clean

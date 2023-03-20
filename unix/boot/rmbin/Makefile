OBJS := rmbin.o

rmbin.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

install: rmbin.e
	cp $? $(bindir)

clean:
	rm -f $(OBJS) rmbin.e

.PHONY: install clean

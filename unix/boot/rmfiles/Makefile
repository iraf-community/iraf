OBJS := rmfiles.o

hbin: rmfiles.e
	cp $? $(hbin)

rmfiles.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

clean:
	rm -f $(OBJS) rmfiles.e

.PHONY: hlib clean

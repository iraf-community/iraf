OBJS := rmbin.o

hbin: rmbin.e
	cp $? $(hbin)

rmbin.e: $(OBJS)
	$(CC) $(LDFLAGS) $? $(LIBS) -o $@

clean:
	rm -f $(OBJS) rmbin.e

.PHONY: hlib clean
